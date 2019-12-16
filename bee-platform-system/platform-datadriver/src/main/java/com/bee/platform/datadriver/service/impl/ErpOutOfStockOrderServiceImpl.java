package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpOutOfStockOrderMapper;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderDTO;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderDetailDTO;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderSearchListDTO;
import com.bee.platform.datadriver.entity.ErpOutOfStockOrder;
import com.bee.platform.datadriver.entity.ErpOutOfStockOrderDetail;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.rq.ErpOutOfStockOrderRQ;
import com.bee.platform.datadriver.rq.ErpOutOfStockSearchRQ;
import com.bee.platform.datadriver.service.ErpOutOfStockOrderDetailService;
import com.bee.platform.datadriver.service.ErpOutOfStockOrderService;
import com.bee.platform.datadriver.service.ErpProductService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 领料出库主表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Slf4j
@Service
public class ErpOutOfStockOrderServiceImpl extends ServiceImpl<ErpOutOfStockOrderMapper, ErpOutOfStockOrder> implements ErpOutOfStockOrderService {


    @Autowired
    private ErpOutOfStockOrderDetailService erpOutOfStockOrderDetailService;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;

    /**
     * 条件搜索领料出库
     *
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpOutOfStockOrderSearchListDTO>> searchOutOfStockOrderByCondition(ErpOutOfStockSearchRQ rq, Page page, Integer companyId) {

        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpOutOfStockOrderSearchListDTO> dto = Lists.newArrayList();
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpOutOfStockOrderServiceImpl", "searchOutOfStockOrderByCondition");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(ids)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        rq.setList(ids);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        dto = baseMapper.searchOutOfStockOrderByCondition(rq,pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));


    }

    /**
     * 删除领料出库
     *
     * @param userInfo
     * @param id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteOutOfStockOrderById(AuthPlatformUserInfo userInfo, Integer id) {

        Date time = new Date();
        Integer userId = userInfo.getId();
        ErpOutOfStockOrder exist = selectOne(new EntityWrapper<ErpOutOfStockOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除领料出库，没有找到相应数据，id为："+id);
            return;
        }

        // 删除主单
        if (!updateById(new ErpOutOfStockOrder().setId(id).setModifierId(userId).setModifyTime(time).setDeleted(1))) {
            log.error("删除领料出库失败，调用{}的{}方法出错", "ErpOutOfStockOrderServiceImpl", "deleteOutOfStockOrderById()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_DELETE_FAILED);
        }
        // 查询详情列表
        List<ErpOutOfStockOrderDetail> details = erpOutOfStockOrderDetailService.selectList(new EntityWrapper<ErpOutOfStockOrderDetail>().eq("out_of_stock_order_id", id).eq("deleted", 0));
        if (CollectionUtils.isEmpty(details)) {
            log.info("没有详情单，主单号为" + id);
            return;
        }

        // 获取详情ids集合
        List<Integer> detailIds = details.stream().map(ErpOutOfStockOrderDetail::getId).collect(Collectors.toList());
        // 依次删除详情
        for (Integer detailId : detailIds) {
            erpOutOfStockOrderDetailService.deleteOutOfStockOrderDetailById(userInfo, detailId);
        }


    }

    /**
     * 修改状态
     *
     * @param userInfo
     * @param id
     * @param state
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateOutOfStockOrderState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpOutOfStockOrder exist = selectOne(new EntityWrapper<ErpOutOfStockOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id修改领料出库状态，没有找到相关数据，id为："+id);
            return;
        }

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpOutOfStockOrder order = new ErpOutOfStockOrder().setId(id).setState(state).setModifierId(userId).setModifyTime(time);
        if (!updateById(order)) {
            log.error("修改领料出库状态失败，调用{}的{}方法出错", "ErpOutOfStockOrderServiceImpl", "updateOutOfStockOrderState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_UPDATE_FAILED);
        }
    }


    /**
     * 保存
     *
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveOutOfStockOrder(AuthPlatformUserInfo userInfo, ErpOutOfStockOrderRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();

        String code = rq.getCode();
        Integer companyId = rq.getCompanyId();
        List<ErpOutOfStockOrder> exist = selectList(new EntityWrapper<ErpOutOfStockOrder>().eq("code", code).eq("company_id", companyId).eq("deleted", 0));
        // 新增校验编号重复
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(exist)) {
            log.error("单号编码重复，编码为:" + code);
            log.error("保存领料出库失败，单号编码重复，调用{}的{}方法出错", "ErpOutOfStockOrderServiceImpl", "saveOutOfStockOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_SAVE_FAILED_CODE_RE);
        }
        // 修改校验编号重复
        if(!ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(exist) && !exist.get(0).getId().equals(rq.getId())){
            log.error("单号编码重复，编码为:" + code);
            log.error("保存领料出库失败，单号编码重复，调用{}的{}方法出错", "ErpOutOfStockOrderServiceImpl", "saveOutOfStockOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_SAVE_FAILED_CODE_RE);
        }

        ErpOutOfStockOrder order = BeanUtils.copyProperties(rq, ErpOutOfStockOrder.class);
        if (ObjectUtils.isEmpty(rq.getId())) {
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId).setState(0);
        } else {
            order.setModifierId(userId).setModifyTime(time).setState(0);
        }

        if (!insertOrUpdate(order)) {
            log.error("保存领料出库失败，调用{}的{}方法出错", "ErpOutOfStockOrderServiceImpl", "saveOutOfStockOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OUT_OF_STOCK_ORDER_SAVE_FAILED);
        }
        return order.getId();
    }

    /**
     * 根据id查询
     *
     * @param id
     * @return
     */
    @Override
    public ErpOutOfStockOrderDTO getOutOfStockOrderById(Integer id) {
        ErpOutOfStockOrderDTO dto = new ErpOutOfStockOrderDTO();

        // 查询主单
        ErpOutOfStockOrder erpOutOfStockOrder = selectOne(new EntityWrapper<ErpOutOfStockOrder>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(erpOutOfStockOrder)) {
            log.info("根据id查看领料出库,没有找到相关数据，id为："+id);
            return dto;
        }
        erpOutOfStockOrder.setCompanyName(commonMapper.getCompanyNameById(erpOutOfStockOrder.getCompanyId()));
        erpOutOfStockOrder.setMaterialBatchName(commonMapper.getMaterialBatchNameById(erpOutOfStockOrder.getMaterialBatchId()));
        erpOutOfStockOrder.setFurnaceNumber(commonMapper.getFurnaceNameById(erpOutOfStockOrder.getFurnaceNumberId()));


        // 查询详情单
        List<ErpOutOfStockOrderDetail> details = erpOutOfStockOrderDetailService.selectList(new EntityWrapper<ErpOutOfStockOrderDetail>().eq("out_of_stock_order_id", id).eq("deleted", 0));
        dto = BeanUtils.copyProperties(erpOutOfStockOrder, ErpOutOfStockOrderDTO.class);
        List<ErpOutOfStockOrderDetailDTO> detailDTOList = BeanUtils.assemble(ErpOutOfStockOrderDetailDTO.class, details);
        for (ErpOutOfStockOrderDetailDTO d : detailDTOList) {
            Integer productId = d.getProductId();
            ErpProduct p = productService.selectById(productId);
            if (!ObjectUtils.isEmpty(p)) {
                d.setProductName(p.getName()).setUnit(p.getUnit());
            }

            d.setStorehouse(commonMapper.getRepositoryNameById(d.getRepositoryId()));
            d.setTestReportCode(commonMapper.getTestCodeById(d.getTestReportId()));
        }

        dto.setDetailDTOList(detailDTOList);

        return dto;
    }
}

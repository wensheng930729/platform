package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
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
import com.bee.platform.datadriver.dao.mapper.ErpOpeningInventoryOrderMapper;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderDTO;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderDetailDTO;
import com.bee.platform.datadriver.dto.ErpOpeningInventorySearchDTO;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrder;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrderDetail;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.entity.ErpProductBatch;
import com.bee.platform.datadriver.rq.ErpOpeningInventoryOrderSaveRQ;
import com.bee.platform.datadriver.rq.ErpOpeningInventorySearchRQ;
import com.bee.platform.datadriver.service.*;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
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
 * 期初库存主表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@Service
public class ErpOpeningInventoryOrderServiceImpl extends ServiceImpl<ErpOpeningInventoryOrderMapper, ErpOpeningInventoryOrder> implements ErpOpeningInventoryOrderService {

    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;

    @Autowired
    private ErpOpeningInventoryOrderDetailService erpOpeningInventoryOrderDetailService;


    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;


    @Autowired
    private ErpProductBatchService productBatchService;

    @Autowired
    private ErpOperationLogService erpOperationLogService;



    /**
     * 条件查询期初库存主表信息
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpOpeningInventorySearchDTO>> searchOpeningInventoryByCondition(ErpOpeningInventorySearchRQ rq, Page page, Integer companyId) {

        Pagination pagination = PageUtils.transFromPage(page);

        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        Wrapper<ErpOpeningInventoryOrder> wrapper = new EntityWrapper<ErpOpeningInventoryOrder>().eq("deleted", 0).orderBy("create_time",false);

        if (!ObjectUtils.isEmpty(rq.getCompanyId())) {
            wrapper.eq("company_id", rq.getCompanyId());
        }
        if (!ObjectUtils.isEmpty(rq.getCode())) {
            wrapper.like("code", rq.getCode());
        }
        if (!ObjectUtils.isEmpty(rq.getStartTime())) {
            wrapper.ge("opening_inventory_time", startTime);
        }
        if (!ObjectUtils.isEmpty(rq.getEndTime())) {
            wrapper.le("opening_inventory_time", endTime);
        }

        List<ErpOpeningInventoryOrder> list = baseMapper.selectPage(pagination,wrapper);
        List<ErpOpeningInventorySearchDTO> dto = BeanUtils.assemble(ErpOpeningInventorySearchDTO.class, list);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }else {
            List<Integer> cIds = dto.stream().map(ErpOpeningInventorySearchDTO::getCompanyId).distinct().collect(Collectors.toList());

            List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();

            if(CollectionUtils.isEmpty(companyList)){
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
            }

            for (ErpOpeningInventorySearchDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO c : companyList) {
                    if(d.getCompanyId().equals(c.getId())){
                        d.setCompanyName(c.getName());
                    }
                }

            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }


    /**
     * 删除期初库存信息
     *
     * @param userInfo
     * @param id
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteOpeningInventoryOrderById(AuthPlatformUserInfo userInfo, Integer id) {

        Date time = new Date();
        Integer userId = userInfo.getId();
        ErpOpeningInventoryOrder exist = selectOne(new EntityWrapper<ErpOpeningInventoryOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除期初库存，没有找到相应数据，id为："+id);
            return;
        }
        // 删除主单
        if (!updateById(new ErpOpeningInventoryOrder().setId(id).setModifierId(userId).setModifyTime(time).setDeleted(1))) {
            log.error("删除期初库存失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "deleteOpeningInventoryOrderById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_DELETE_FAILED);
        }
        List<ErpOpeningInventoryOrderDetail> details = erpOpeningInventoryOrderDetailService.selectList(new EntityWrapper<ErpOpeningInventoryOrderDetail>().eq("opening_inventory_order_id", id).eq("deleted", 0));
        if (CollectionUtils.isEmpty(details)) {
            log.info("根据id删除期初库存明细时，没有找到相应数据，主单id为："+id);
            return;
        }
        // 删除详情
        // 根据主单id查询所有详情
        // 获取详情ids集合
        List<Integer> detailIds = details.stream().map(ErpOpeningInventoryOrderDetail::getId).collect(Collectors.toList());
        // 依次删除详情
        for (Integer i : detailIds) {
            erpOpeningInventoryOrderDetailService.deleteOpeningInventoryOrderDetailById(userInfo, i);
        }

    }


    /**
     * 保存期初库存
     *
     * @param userInfo
     * @param rq
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveOpeningInventoryOrder(AuthPlatformUserInfo userInfo, ErpOpeningInventoryOrderSaveRQ rq) {

        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        String code = rq.getCode();
        Integer companyId = rq.getCompanyId();
        List<ErpOpeningInventoryOrder> exist = selectList(new EntityWrapper<ErpOpeningInventoryOrder>().eq("code", code).eq("company_id", companyId).eq("deleted", 0));
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(exist)) {
            log.info("单号编码重复，编码为:" + code);
            log.info("保存期初库存失败，单号编码重复，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED_CODE_RE);
        }
        // 如果id不为空为修改
        // 判断原始记录和传入记录code值相等
        // 相等则不判断code是否重复  否则查询数据库判断code是否重复
        if(!ObjectUtils.isEmpty(rq.getId())&& !CollectionUtils.isEmpty(exist) && !exist.get(0).getId().equals(rq.getId())){
            log.info("单号编码重复，编码为:" + code);
            log.info("保存期初库存失败，单号编码重复，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED_CODE_RE);
        }

        ErpOpeningInventoryOrder order = BeanUtils.copyProperties(rq, ErpOpeningInventoryOrder.class);
        String msg;
        if (ObjectUtils.isEmpty(rq.getId())) {
            msg="新增";
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId).setState(0);
        } else {
            msg="编辑";
            order.setModifierId(userId).setModifyTime(time).setState(0);
        }
        if (!insertOrUpdate(order)) {
            log.error("保存期初库存失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED);
        }
        // 保存操作日志
        erpOperationLogService.saveLog(order.getCompanyId(),userInfo,order.getId(),"init_stock",msg);

        return order.getId();
    }

    /**
     * 修改期初库存状态
     *
     * @param userInfo
     * @param id
     * @param state
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateOpeningInventoryState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpOpeningInventoryOrder exist = selectOne(new EntityWrapper<ErpOpeningInventoryOrder>().eq("id", id).eq("deleted", 0));
        if(ObjectUtils.isEmpty(exist)){

            log.info("根据id修改期初库存状态，没有找到相关数据，id为："+id);
            return;
        }
        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpOpeningInventoryOrder order = new ErpOpeningInventoryOrder().setId(id).setState(state).setModifierId(userId).setModifyTime(time);
        if (!updateById(order)) {
            log.error("修改期初库存状态失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "updateOpeningInventoryState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_UPDATE_FAILED);
        }
    }

    /**
     * 根据id查看期初库存详情
     *
     * @param id
     * @return
     */
    @Override
    public ErpOpeningInventoryOrderDTO getOpeningInventoryById(Integer id) {
        ErpOpeningInventoryOrderDTO dto = new ErpOpeningInventoryOrderDTO();
        // 查询主单
        ErpOpeningInventoryOrder erpOpeningInventoryOrder = selectOne(new EntityWrapper<ErpOpeningInventoryOrder>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(erpOpeningInventoryOrder)) {
            log.info("根据id查看期初库存,没有找到相关数据，id为："+id);
            return dto;
        }
        Integer companyId = erpOpeningInventoryOrder.getCompanyId();

        // 设置企业名称
        String companyName =null;
        List<AuthEnterpriseFeignDetailDTO> companyNames = authEnterpriseFeignClient.getEnterpriseMoreDetail(Lists.newArrayList(companyId)).getObject();
        if(!CollectionUtils.isEmpty(companyNames)){
             companyName = companyNames.get(0).getName();
        }

        erpOpeningInventoryOrder.setCompanyName(companyName);
        // 查询详情单
        List<ErpOpeningInventoryOrderDetail> details = erpOpeningInventoryOrderDetailService.selectList(new EntityWrapper<ErpOpeningInventoryOrderDetail>().eq("opening_inventory_order_id", id).eq("deleted", 0));
        dto = BeanUtils.copyProperties(erpOpeningInventoryOrder, ErpOpeningInventoryOrderDTO.class);
        List<ErpOpeningInventoryOrderDetailDTO> detailDTOList = BeanUtils.assemble(ErpOpeningInventoryOrderDetailDTO.class, details);
        for (ErpOpeningInventoryOrderDetailDTO d : detailDTOList) {
            ErpProduct p = productService.selectById(d.getProductId());
            ErpProductBatch pb = productBatchService.selectById(d.getProductBatchId());
            if (!ObjectUtils.isEmpty(p)) {
                d.setProductName(p.getName()).setUnitOfMeasurement(p.getUnit());
            }
            if (!ObjectUtils.isEmpty(pb)) {
                d.setProductAndBatch(p.getName()+"-"+pb.getBatchName());
            }else if(!ObjectUtils.isEmpty(p) && ObjectUtils.isEmpty(pb)) {
                d.setProductAndBatch(p.getName());
            }
            d.setStoreHouseName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
            d.setTestOrder(commonMapper.getTestCodeById(d.getTestReportId()));
        }
        dto.setDetailDTOList(detailDTOList);
        return dto;
    }
}

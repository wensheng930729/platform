package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dao.mapper.ErpOpeningInventoryOrderMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrder;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrderDetail;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.rq.ErpOpeningInventoryOrderSaveRQ;
import com.bee.platform.datadriver.rq.ErpOpeningInventorySearchRQ;
import com.bee.platform.datadriver.rq.OpeningInventoryOrderQueryRQ;
import com.bee.platform.datadriver.service.ErpOpeningInventoryOrderDetailService;
import com.bee.platform.datadriver.service.ErpOpeningInventoryOrderService;
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

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
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
    private ErpOpeningInventoryOrderMapper openingInventoryOrderMapper;
    private static Integer ZERO = 0;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpProductService productService;

    /**
     * 条件查询期初库存列表
     *
     * @param
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<List<ErpOpeningInventoryOrderSearchListDTO>> searchOpeningInventoryOrderByCondition(AuthPlatformUserInfo userInfo, OpeningInventoryOrderQueryRQ rq, Page page, String sysToken) {

        Pagination pagination = PageUtils.transFromPage(page);
//        Map<String, Object> map = new HashMap<>();
//        if (!ObjectUtils.isEmpty(rq)) {
//            if (!StringUtils.isEmpty(rq.getCompany())) {
//                map.put("company", rq.getCompany());
//            }
//            if (!StringUtils.isEmpty(rq.getProduct())) {
//                map.put("product", rq.getProduct());
//            }
//            if (!StringUtils.isEmpty(rq.getCreateStartTime())) {
//                map.put("createStartTime", rq.getCreateStartTime());
//            }
//            if (!StringUtils.isEmpty(rq.getCreateEndTime())) {
//                map.put("createEndTime", rq.getCreateEndTime());
//            }
//        }
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
//        List<Integer> id = enterpriseFlatDTOS.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList());

        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpOpeningInventoryOrderServiceImpl", "searchOpeningInventoryOrderByCondition");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        List<ErpOpeningInventoryOrderSearchListDTO> dto = Lists.newArrayList();
        if (CollectionUtils.isEmpty(ids)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
//        List<Integer> ids= Lists.newArrayList(1,2,3);
        rq.setList(ids);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
//            startTime = startTime + " 00:00:00";
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
//            endTime = endTime + " 23:59:59";
            rq.setEndTime(endTime + " 23:59:59");
        }

        dto = openingInventoryOrderMapper.selectInventoryOrderByCondition(rq, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

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

        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpOpeningInventoryOrderServiceImpl", "searchOpeningInventoryOrderByCondition");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(),PageUtils.transToPage(pagination));
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList());
        List<ErpOpeningInventorySearchDTO> dto = Lists.newArrayList();
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

        Wrapper<ErpOpeningInventoryOrder> wrapper = new EntityWrapper<ErpOpeningInventoryOrder>().eq("deleted", 0);

        if (ObjectUtils.isEmpty(rq.getCompanyId())) {
            wrapper.in("company_id", ids);
        }
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
        dto = BeanUtils.assemble(ErpOpeningInventorySearchDTO.class, list);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }else {
            List<Integer> cIds = dto.stream().map(ErpOpeningInventorySearchDTO::getCompanyId).collect(Collectors.toList());
            List<GetCompanyNameByIdsDTO> companyNames = commonMapper.getCompanyNameByIds(cIds);
            if(CollectionUtils.isEmpty(companyNames)){
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
            }

            for (ErpOpeningInventorySearchDTO d : dto) {
                for (GetCompanyNameByIdsDTO c : companyNames) {
                    if(d.getCompanyId().equals(c.getCompanyId())){
                        d.setCompanyName(c.getCompanyName());
                    }
                }

            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 获取期初库存信息
     *
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpOpeningInventoryOrder> getOpeningInventoryOrder(AuthPlatformUserInfo userInfo, String id) {
        ErpOpeningInventoryOrder inventoryOrder = openingInventoryOrderMapper.selectById(id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, inventoryOrder);
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
     * @param userInfo
     * @param id
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<Integer> confirmOpeningInventoryOrder(AuthPlatformUserInfo userInfo, String id) {
        if (openingInventoryOrderMapper.updateById(new ErpOpeningInventoryOrder()
                .setId(Integer.valueOf(id)).setState(Status.TRUE.getKey()).setModifierId(userInfo.getId()).setModifyTime(new Date())) <= ZERO) {
            log.error("确认期初库存失败,调用{}类{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "confirmOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
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
            log.error("单号编码重复，编码为:" + code);
            log.error("保存期初库存失败，单号编码重复，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED_CODE_RE);
        }
        // 如果id不为空为修改
        // 判断原始记录和传入记录code值相等
        // 相等则不判断code是否重复  否则查询数据库判断code是否重复
        if(!ObjectUtils.isEmpty(rq.getId())&& !CollectionUtils.isEmpty(exist) && !exist.get(0).getId().equals(rq.getId())){
            log.error("单号编码重复，编码为:" + code);
            log.error("保存期初库存失败，单号编码重复，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED_CODE_RE);
        }


        ErpOpeningInventoryOrder order = BeanUtils.copyProperties(rq, ErpOpeningInventoryOrder.class);
        if (ObjectUtils.isEmpty(rq.getId())) {
            order.setCreateTime(time).setCreatorId(userId).setCreatorEnterpriseId(orgId).setState(0);
        } else {
            order.setModifierId(userId).setModifyTime(time).setState(0);
        }
        if (!insertOrUpdate(order)) {
            log.error("保存期初库存失败，调用{}的{}方法出错", "ErpOpeningInventoryOrderServiceImpl", "saveOpeningInventoryOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED);
        }
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
        String companyName = commonMapper.getCompanyNameById(companyId);
        erpOpeningInventoryOrder.setCompanyName(companyName);

        // 查询详情单
        List<ErpOpeningInventoryOrderDetail> details = erpOpeningInventoryOrderDetailService.selectList(new EntityWrapper<ErpOpeningInventoryOrderDetail>().eq("opening_inventory_order_id", id).eq("deleted", 0));
        dto = BeanUtils.copyProperties(erpOpeningInventoryOrder, ErpOpeningInventoryOrderDTO.class);
        List<ErpOpeningInventoryOrderDetailDTO> detailDTOList = BeanUtils.assemble(ErpOpeningInventoryOrderDetailDTO.class, details);
        for (ErpOpeningInventoryOrderDetailDTO d : detailDTOList) {


            Integer productId = d.getProductId();
            ErpProduct p = productService.selectById(productId);
            if (!ObjectUtils.isEmpty(p)) {
                d.setProductName(p.getName()).setUnitOfMeasurement(p.getUnit());
            }

            d.setStoreHouseName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
            d.setTestOrder(commonMapper.getTestCodeById(d.getTestReportId()));

        }

        dto.setDetailDTOList(detailDTOList);

        return dto;
    }
}

package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
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
import com.bee.platform.datadriver.dao.mapper.ErpRepoReceiptDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpRepositoryReceiptMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.enums.EnumErpSaleOrderStatus;
import com.bee.platform.datadriver.rq.*;
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

import java.math.BigDecimal;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpRepositoryReceiptServiceImpl extends ServiceImpl<ErpRepositoryReceiptMapper, ErpRepositoryReceipt> implements ErpRepositoryReceiptService {

    @Autowired
    private ErpRepoReceiptDetailService repoReceiptDetailService;
    @Autowired
    private ErpRepoReceiptDetailMapper repoReceiptDetailMapper;

    @Autowired
    private ErpPurchaseOrderService erpPurchaseOrderService;

    @Autowired
    private ErpPurchaseOrderDetailService erpPurchaseOrderDetailService;

    @Autowired
    private ErpSaleOrderService erpSaleOrderService;

    @Autowired
    private CommonMapper commonMapper;

    @Autowired
    private ErpOperationLogService erpOperationLogService;

    @Autowired
    private ErpSaleOrderDetailService saleOrderDetailService;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Override
    public ResponseResult<List<ErpInventoryFlowDTO>> searchInventoryFlowByCondition(ErpInventoryFlowSearchRQ rq, Page page, Integer companyId) {

        Integer type = rq.getType();
        Pagination pagination = PageUtils.transFromPage(page);

        List<Integer> typeList = Lists.newArrayList(0, 1, 2, 3);
        if (!typeList.contains(type)) {
            log.info("type类型不匹配，type为：{}" + type);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }

        List<ErpInventoryFlowDTO> dto = baseMapper.searchInventoryFlowByCondition(pagination, rq);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpInventoryFlowDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
    }


    /**
     * 保存原料入库主单
     *
     * @param userInfo
     * @param rq
     * @return
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveRepositoryReceiptRawIn(AuthPlatformUserInfo userInfo, RepositoryReceiptRawInRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();


        ErpRepositoryReceipt order = BeanUtils.copyProperties(rq, ErpRepositoryReceipt.class);
        Integer relatedOrderId = rq.getRelatedOrderId();
        // 查询源采购订单
        ErpPurchaseOrder erpPurchaseOrder = erpPurchaseOrderService.selectById(relatedOrderId);
        if (ObjectUtils.isEmpty(erpPurchaseOrder)) {
            log.info("保存原料入库失败，没有查到相关的源采购订单信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_NO_PU_ORDER);
        }
        Integer companyId = erpPurchaseOrder.getCompany();
        if (ObjectUtils.isEmpty(companyId)) {
            log.info("保存原料入库失败，没有查到相关的源采购订单中 的公司信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_NO_COMPANY_ID);
        }

        String code = rq.getCode();
        List<ErpRepositoryReceipt> existCode = selectList(new EntityWrapper<ErpRepositoryReceipt>().eq("code", code).eq("org_id", companyId).eq("deleted", 0).eq("type", "material_stock"));
        // 新增校验编号重复
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode)) {
            log.info("单号编码重复，编码为:" + code);
            log.info("保存原料入库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_CODE_RE);
        }
        // 修改校验编号重复
        if (!ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode) && !existCode.get(0).getId().equals(rq.getId())) {
            log.info("单号编码重复，编码为:" + code);
            log.info("保存原料入库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_CODE_RE);

        }

        // 将源采购订单的公司id写入原料入库的公司id
        order.setOrgId(companyId);
        // 设置业务类型 为原料入库
        order.setType(EnumBusinessType.MATERIAL_STOCK.getCode());
        String msg;
        if (ObjectUtils.isEmpty(rq.getId())) {
            msg = "新增";
            order.setCreateTime(time).setCreateUser(userId).setEnterpriseId(orgId).setState(0);
        } else {
            msg = "编辑";
            order.setUpdateUser(userId).setUpdateTime(time).setState(0);
        }

        if (!insertOrUpdate(order)) {
            log.error("保存原料入库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED);
        }

        // 保存操作日志
        erpOperationLogService.saveLog(orgId, userInfo, order.getId(), "material_stock", msg);

        return order.getId();


    }


    /**
     * 修改原料入库确认状态
     *
     * @param userInfo
     * @param id
     * @param state
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateRepositoryReceiptRawInState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("根据id修改原料入库状态，没有找到相关数据，id为：" + id);
            return;
        }
        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpRepositoryReceipt order = new ErpRepositoryReceipt().setId(id).setState(state).setUpdateUser(userId).setUpdateTime(time);
        if (!updateById(order)) {
            log.error("修改原料入库状态失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "updateRepositoryReceiptRawInState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_UPDATE_FAILED);
        }

    }

    /**
     * 删除原料入库单
     *
     * @param userInfo
     * @param id
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteRepositoryReceiptRawInById(AuthPlatformUserInfo userInfo, Integer id) {

        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("删除原料入库单时，根据id没有找到相关记录，id为:" + id);
            return;
        }

        // 删除主单
        if (!updateById(new ErpRepositoryReceipt().setId(id).setDeleted(1))) {
            log.error("删除原料入库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "deleteRepositoryReceiptRawInById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_DELETE_FAILED);
        }

        // 根据主单id查询所有详情
        List<ErpRepoReceiptDetail> details = repoReceiptDetailService.selectList(new EntityWrapper<ErpRepoReceiptDetail>().eq("receipt_id", id).eq("deleted", 0));
        if (CollectionUtils.isEmpty(details)) {
            log.info("删除原料入库单时，根据id没有找到相关明细单记录，id为:" + id);
            return;
        }

        // 删除详情
        // 获取详情ids集合
        List<Integer> detailIds = details.stream().map(ErpRepoReceiptDetail::getId).collect(Collectors.toList());
        // 依次删除详情
        for (Integer i : detailIds) {
            repoReceiptDetailService.deleteRepoReceiptDetailRawIn(userInfo, i);
        }


    }

    /**
     * 根据id查询 原料入库信息
     *
     * @param id
     * @return
     */
    @Override
    public ErpRepositoryReceiptRawInDTO getRepositoryReceiptRawInById(Integer id) {
        ErpRepositoryReceiptRawInDTO dto = new ErpRepositoryReceiptRawInDTO();
        // 查询主表信息
        ErpRepositoryReceipt one = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(one)) {
            log.info("根据id查看原料入库,没有找到相关数据，id为：" + id);
            return dto;
        }
        Integer relatedOrderId = one.getRelatedOrderId();
        one.setRelatedOrder(commonMapper.getPurchaseOrderCodeById(relatedOrderId));

        dto = BeanUtils.copyProperties(one, ErpRepositoryReceiptRawInDTO.class);
        // 查询详情表信息
        List<ErpRepoReceiptDetailRawInDTO> detailRawInDTOList = repoReceiptDetailService.getRepoReceiptDetailRawInList(id);
        // 查询采购详情列表
        List<ErpPurchaseOrderDetail> purchaseOrderDetails = erpPurchaseOrderDetailService.selectList(new EntityWrapper<ErpPurchaseOrderDetail>().eq("order_id", relatedOrderId).eq("deleted", 0));
        if(!CollectionUtils.isEmpty(detailRawInDTOList)&&!CollectionUtils.isEmpty(purchaseOrderDetails)){
            for (ErpRepoReceiptDetailRawInDTO rd : detailRawInDTOList) {

                for (ErpPurchaseOrderDetail pd : purchaseOrderDetails) {
                    if(rd.getProductId()!=null && pd.getProductId()!=null && rd.getProductId().equals(pd.getProductId())&&rd.getProductBatchId()!=null && pd.getBatchId()!=null && rd.getProductBatchId().equals(pd.getBatchId())){
                        // 设置合同数量
                        rd.setContractNum(pd.getNum());
                    }
                    if(rd.getProductId() !=null && pd.getProductId() !=null && rd.getProductId().equals(pd.getProductId())&&rd.getProductBatchId()==null && pd.getBatchId()==null){
                        // 设置合同数量
                        rd.setContractNum(pd.getNum());
                    }
                }
            }
        }

//        for (ErpRepoReceiptDetailRawInDTO d : detailRawInDTOList) {
//            Integer productId = d.getProductId();
//            ErpProduct p = productService.selectById(productId);
//            ErpProductBatch pb = productBatchService.selectById(d.getProductBatchId());
//            if (!ObjectUtils.isEmpty(p)) {
//                d.setProductName(p.getName()).setUnit(p.getUnit());
//            }
//            if (!ObjectUtils.isEmpty(pb)) {
//                d.setProductAndBatch(p.getName()+"-"+pb.getBatchName());
//            }else {
//                d.setProductAndBatch(p.getName());
//            }
//
//            d.setRepositoryName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
//            d.setTestCode(commonMapper.getTestCodeById(d.getTestId()));
//            d.setResult(commonMapper.getTestResultById(d.getTestId()));
//
//        }


        dto.setDetailRawInDTOList(detailRawInDTOList);

        return dto;
    }

    /**
     * 保存成品出库信息
     *
     * @param userInfo
     * @param rq
     * @return
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveRepositoryReceiptProductOut(AuthPlatformUserInfo userInfo, ErpRepositoryReceiptProductOutRQ rq) {
        Date time = new Date();
        Integer orgId = userInfo.getOrgId();
        Integer userId = userInfo.getId();
        ErpRepositoryReceipt order = BeanUtils.copyProperties(rq, ErpRepositoryReceipt.class);
        Integer relatedOrderId = rq.getRelatedOrderId();
        // 查询源销售订单
        ErpSaleOrder erpSaleOrder = erpSaleOrderService.selectById(relatedOrderId);
        if (ObjectUtils.isEmpty(erpSaleOrder)) {
            log.info("保存成品出库失败，没有查到相关的源销售订单信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_NO_SALE_ORDER);
        }
        Integer companyId = erpSaleOrder.getCompany();
        if (ObjectUtils.isEmpty(companyId)) {
            log.info("保存成品出库失败，没有查到相关的源销售订单中 的公司信息 调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_NO_COMPANY_ID);
        }

        String code = rq.getCode();
//        Integer companyId = rq.getCompanyId();
        List<ErpRepositoryReceipt> existCode = selectList(new EntityWrapper<ErpRepositoryReceipt>().eq("code", code).eq("org_id", companyId).eq("deleted", 0).eq("type", "product_delivery"));
        // 新增校验编号重复
        if (ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode)) {
            log.info("单号编码重复，编码为{}" + code);
            log.info("保存成品出库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_CODE_RE);
        }
        // 修改校验编号重复
        if (!ObjectUtils.isEmpty(rq.getId()) && !CollectionUtils.isEmpty(existCode) && !existCode.get(0).getId().equals(rq.getId())) {
            log.info("单号编码重复，编码为{}" + code);
            log.info("保存原料入库失败，单号编码重复，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptRawIn()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_CODE_RE);

        }


        // 将源销售订单的公司id写入成品出库的公司id
        order.setOrgId(companyId);
        // 设置业务类型 为成品出库
        order.setType(EnumBusinessType.PRODUCT_DELIVERY.getCode());
        String msg;
        if (ObjectUtils.isEmpty(rq.getId())) {
            msg = "新增";
            order.setCreateTime(time).setCreateUser(userId).setEnterpriseId(orgId).setState(0);
        } else {
            msg = "编辑";
            order.setUpdateUser(userId).setUpdateTime(time).setState(0);
        }

        if (!insertOrUpdate(order)) {
            log.error("保存成品出库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "saveRepositoryReceiptProductOut()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED);
        }

        // 保存操作日志
        erpOperationLogService.saveLog(orgId, userInfo, order.getId(), "product_delivery", msg);

        return order.getId();

    }

    /**
     * 修改成品出库状态
     *
     * @param userInfo
     * @param id
     * @param state
     */

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateRepositoryReceiptProductOutState(AuthPlatformUserInfo userInfo, Integer id, Integer state) {
        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("根据id修改成品出库状态，没有找到相关数据，id为：" + id);
            return;
        }

        Integer userId = userInfo.getId();
        Date time = new Date();
        ErpRepositoryReceipt order = new ErpRepositoryReceipt().setId(id).setState(state).setUpdateUser(userId).setUpdateTime(time);
        if (!updateById(order)) {
            log.error("修改原料入库状态失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "updateRepositoryReceiptProductOutState()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_RAW_IN_UPDATE_FAILED);
        }

    }

    /**
     * 删除成品出库单
     *
     * @param userInfo
     * @param id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteRepositoryReceiptProductOutById(AuthPlatformUserInfo userInfo, Integer id) {
        ErpRepositoryReceipt exist = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(exist)) {
            log.info("删除成品出库单时，根据id没有找到相关记录，id为:" + id);
            return;
        }

        // 删除主单
        if (!updateById(new ErpRepositoryReceipt().setId(id).setDeleted(1))) {
            log.error("删除原料入库失败，调用{}的{}方法出错", "ErpRepositoryReceiptServiceImpl", "deleteRepositoryReceiptRawInById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DELETE_FAILED);
        }

        // 根据主单id查询所有详情
        List<ErpRepoReceiptDetail> details = repoReceiptDetailService.selectList(new EntityWrapper<ErpRepoReceiptDetail>().eq("receipt_id", id).eq("deleted", 0));
        if (CollectionUtils.isEmpty(details)) {
            log.info("删除成品出库单时，根据id没有找到相关明细单记录，id为:" + id);
            return;
        }

        // 删除详情
        // 获取详情ids集合
        List<Integer> detailIds = details.stream().map(ErpRepoReceiptDetail::getId).collect(Collectors.toList());
        // 依次删除详情
        for (Integer i : detailIds) {
            repoReceiptDetailService.deleteRepoReceiptDetailProductOutById(userInfo, i);
        }

        //修改销售订单发货状态
        erpSaleOrderService.updateById(new ErpSaleOrder().setId(exist.getRelatedOrderId()).setDeliveryState(EnumErpSaleOrderStatus.DeliveryType.N0T_DELIVERY.getKey()));

    }

    /**
     * 查询成品出库信息
     *
     * @param id
     * @return
     */
    @Override
    public ErpRepositoryReceiptProductOutDTO getRepositoryReceiptProductOutById(Integer id) {
        ErpRepositoryReceiptProductOutDTO dto = new ErpRepositoryReceiptProductOutDTO();

        // 查询主表信息
        ErpRepositoryReceipt one = selectOne(new EntityWrapper<ErpRepositoryReceipt>().eq("id", id).eq("deleted", 0));
        if (ObjectUtils.isEmpty(one)) {
            log.info("根据id查看成品出库,没有找到相关数据，id为：" + id);
            return dto;
        }
        one.setRelatedOrder(commonMapper.getSaleOrderCodeById(one.getRelatedOrderId()));


        dto = BeanUtils.copyProperties(one, ErpRepositoryReceiptProductOutDTO.class);
        // 查询详情表信息
        List<ErpRepoReceiptDetailProductOutDTO> detailProductOutDTOList = repoReceiptDetailService.getRepoReceiptDetailProductOutList(id);
//        for (ErpRepoReceiptDetailProductOutDTO d : detailProductOutDTOList) {
//            Integer productId = d.getProductId();
//            ErpProduct p = productService.selectById(productId);
//            if(!ObjectUtils.isEmpty(p)){
//                d.setProductName(p.getName()).setUnit(p.getUnit());
//            }
//
//            d.setRepositoryName(commonMapper.getRepositoryNameById(d.getRepositoryId()));
//            d.setTestCode(commonMapper.getTestCodeById(d.getTestId()));
//            d.setResult(commonMapper.getTestResultById(d.getTestId()));
//
//        }

        dto.setDetailProductOutDTOList(detailProductOutDTOList);

        return dto;
    }

    /**
     * 条件搜索原料入库列表
     *
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpRepositoryReceiptRawInSearchDTO>> searchRepositoryReceiptRawInByCondition(ErpRepositoryReceiptRawInSearchRQ rq, Page page, Integer companyId) {
        Pagination pagination = PageUtils.transFromPage(page);
//        List<ErpRepositoryReceiptRawInSearchDTO> dto = Lists.newArrayList();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryReceiptServiceImpl", "searchRepositoryReceiptRawInByCondition");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        if(CollectionUtils.isEmpty(ids)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
//        }
//        rq.setList(ids);
        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }
        List<ErpRepositoryReceiptRawInSearchDTO> dto = baseMapper.searchRepositoryReceiptRawInByCondition(rq, pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpRepositoryReceiptRawInSearchDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 条件搜索成品出库列表
     *
     * @param
     * @param rq
     * @param page
     * @param
     * @return
     */
    @Override
    public ResponseResult<List<ErpRepositoryReceiptProductOutSearchDTO>> searchRepositoryReceiptProductOutByCondition(ErpRepositoryReceiptProductOutSearchRQ rq, Page page, Integer companyId) {

        Pagination pagination = PageUtils.transFromPage(page);
//        List<ErpRepositoryReceiptProductOutSearchDTO> dto = Lists.newArrayList();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryReceiptServiceImpl", "searchRepositoryReceiptProductOutByCondition");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList(), PageUtils.transToPage(pagination));
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        if(CollectionUtils.isEmpty(ids)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
//        }
//        rq.setList(ids);
        rq.setCompanyId(companyId);
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }


        List<ErpRepositoryReceiptProductOutSearchDTO> dto = baseMapper.searchRepositoryReceiptProductOutByCondition(rq, pagination);
        if(CollectionUtils.isEmpty(dto)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        List<Integer> cIds = dto.stream().map(o->o.getCompanyId()).distinct().collect(Collectors.toList());
        // 设置企业名称
        List<AuthEnterpriseFeignDetailDTO> companyList = authEnterpriseFeignClient.getEnterpriseMoreDetail(cIds).getObject();
        if(!CollectionUtils.isEmpty(companyList)){
            for (ErpRepositoryReceiptProductOutSearchDTO d : dto) {
                for (AuthEnterpriseFeignDetailDTO e : companyList) {
                    if(d.getCompanyId().equals(e.getId())){
                        d.setCompanyName(e.getName());
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * @Description 根据销售订单id查看销售发货情况
     * @Param id
     * @Date 2019/6/12 18:24
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<RepoReceiptDetailDTO>> getSaleDeliveryInfo(Integer id) {
        //查询发货情况
        List<RepoReceiptDetailDTO> saleDeliveryList = repoReceiptDetailMapper.getSaleDeliveryInfo(id);
        if (!CollectionUtils.isEmpty(saleDeliveryList)) {
            //查询销售订单产品合同重量信息
            List<ErpSaleOrderDetail> saleOrderDetails = saleOrderDetailService
                    .selectList(new EntityWrapper<>(new ErpSaleOrderDetail()
                    .setOrderId(id)
                    .setDeleted(Status.FALSE.getKey())));
            Map<Integer, BigDecimal> productNumMap = new HashMap<>();
            if (!CollectionUtils.isEmpty(saleOrderDetails)) {
                saleOrderDetails.forEach(order -> {
                    BigDecimal num = productNumMap.get(order.getProductBatchId());
                    if (ObjectUtils.isEmpty(num)) {
                        num = BigDecimal.ZERO;
                    }
                    num = num.add(order.getNum());
                    productNumMap.put(order.getProductBatchId(), num);
                });
            }
            //设置合同重量
            saleDeliveryList.forEach(delivery -> {
                delivery.setOrderNum(productNumMap.get(delivery.getProductBatchId()));
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleDeliveryList);
    }
}

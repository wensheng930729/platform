package com.bee.platform.datadriver.service.impl;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumErpSaleOrderStatus;
import com.bee.platform.datadriver.enums.EnumInvoiceStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseInvoiceOrderDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseInvoiceOrderMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderMapper;
import com.bee.platform.datadriver.dto.ErpPurchaseInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.rq.ErpPurchaseInvoiceOrderSaveRQ;
import com.bee.platform.datadriver.rq.InvoiceOrderQueryRQ;
import com.bee.platform.datadriver.rq.OrderDeleteRQ;
import com.bee.platform.datadriver.service.ErpPurchaseInvoiceOrderService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 采购发票 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Slf4j
@Service
public class ErpPurchaseInvoiceOrderServiceImpl extends ServiceImpl<ErpPurchaseInvoiceOrderMapper, ErpPurchaseInvoiceOrder> implements ErpPurchaseInvoiceOrderService {

    @Autowired
    private ErpPurchaseInvoiceOrderMapper purchaseInvoiceOrderMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpPurchaseInvoiceOrderDetailMapper invoiceOrderDetailMapper;

    @Autowired
    private ErpPurchaseOrderMapper purchaseOrderMapper;
    private static Integer ZERO = 0;

    /**
     * 分页查询采购发票列表
     * @param pagination
     * @param rq
     * @return
     */
    @Override
    public List<ErpPurchaseInvoiceOrderInfoDTO> listPurchaseInvoiceOrder(Pagination pagination, InvoiceOrderQueryRQ rq, Integer companyId) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpPurchaseInvoiceOrderServiceImpl", "listPurchaseInvoiceOrder");
            return Lists.newArrayList();
        }
        List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if (!StringUtils.isEmpty(rq.getCreateStartTime()) && DateUtils.isValidDate(rq.getCreateStartTime())){
            rq.setCreateStartTime(rq.getCreateStartTime() + " 00:00:00");
        }
        if (!StringUtils.isEmpty(rq.getCreateEndTime()) && DateUtils.isValidDate(rq.getCreateEndTime())){
            rq.setCreateEndTime(rq.getCreateEndTime() + " 23:59:59");
        }
        rq.setList(enterpriseIds);
        return purchaseInvoiceOrderMapper.selectInvoiceOrderByCondition(rq,pagination);
    }

    /**
     * 获取采购发票信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpPurchaseInvoiceOrder> getPurchaseInvoiceOrder(String id) {
        ErpPurchaseInvoiceOrder invoiceOrder = purchaseInvoiceOrderMapper
                .selectById(new ErpPurchaseInvoiceOrder().setId(Integer.valueOf(id)).setDeleted(Status.FALSE.getKey()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,invoiceOrder);
    }

    /**
     * 删除采购发票信息
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deletePurchaseInvoiceOrder(AuthPlatformUserInfo userInfo, OrderDeleteRQ rq) {
        /*if (rq.getSubId() != null){
            ErpPurchaseInvoiceOrderDetail detail = invoiceOrderDetailMapper.selectById(new ErpPurchaseInvoiceOrderDetail()
                    .setId(rq.getSubId()).setDeleted(Status.FALSE.getKey()));
            if (invoiceOrderDetailMapper.updateById(new ErpPurchaseInvoiceOrderDetail()
                    .setId(rq.getSubId()).setDeleted(Status.TRUE.getKey())) <= ZERO){
                log.error("删除采购发票明细失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","deletePurchaseInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DETAIL_DELETE_FAILED);
            }
            if (invoiceOrderDetailMapper.selectCount(new EntityWrapper<>(new ErpPurchaseInvoiceOrderDetail()
                    .setOrderId(detail.getOrderId()).setDeleted(Status.FALSE.getKey()))) <= ZERO){
                if (purchaseInvoiceOrderMapper.updateById(new ErpPurchaseInvoiceOrder()
                        .setId(detail.getOrderId()).setDeleted(Status.TRUE.getKey())
                        .setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
                    log.error("删除采购发票失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","deletePurchaseInvoiceOrder()");
                    throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DELETE_FAILED);
                }
            }
        }else {
            if (purchaseInvoiceOrderMapper.updateById(new ErpPurchaseInvoiceOrder()
                    .setId(rq.getId()).setDeleted(Status.TRUE.getKey())
                    .setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
                log.error("删除采购发票失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","deletePurchaseInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DELETE_FAILED);
            }
        }*/
        ErpPurchaseInvoiceOrder invoiceOrder = purchaseInvoiceOrderMapper.selectById(new ErpPurchaseInvoiceOrder()
                .setId(rq.getId()).setDeleted(Status.FALSE.getKey()));
        if (purchaseInvoiceOrderMapper.updateById(new ErpPurchaseInvoiceOrder()
                .setId(rq.getId()).setDeleted(Status.TRUE.getKey())
                .setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
            log.error("删除采购发票失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","deletePurchaseInvoiceOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DELETE_FAILED);
        }
        if (invoiceOrderDetailMapper.update(new ErpPurchaseInvoiceOrderDetail()
                .setDeleted(Status.TRUE.getKey()),new EntityWrapper<>(new ErpPurchaseInvoiceOrderDetail().setOrderId(rq.getId()))) < ZERO){
            log.error("删除采购发票明细失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","deletePurchaseInvoiceOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_DETAIL_DELETE_FAILED);
        }
        updatePurchaseOrderState(invoiceOrder.getPurchaseOrder());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,rq.getId());
    }

    /**
     * 保存采购发票
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<Integer> savePurchaseInvoiceOrder(AuthPlatformUserInfo userInfo, ErpPurchaseInvoiceOrderSaveRQ rq) {
        ErpPurchaseInvoiceOrder invoiceOrder = BeanUtils.copyProperties(rq,ErpPurchaseInvoiceOrder.class);
        // 新增
        if (invoiceOrder.getId() == null){
            if (!ObjectUtils.isEmpty(purchaseInvoiceOrderMapper.selectOne(new ErpPurchaseInvoiceOrder()
                    .setCode(rq.getCode())
                    .setCompany(rq.getCompany())
                    .setDeleted(Status.FALSE.getKey())))){
                log.error("发票编号重复", "ErpPurchaseInvoiceOrderServiceImpl", "savePurchaseInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_CODE_REPEAT);
            }
            if (purchaseInvoiceOrderMapper.insert(invoiceOrder
                    .setState(Status.FALSE.getKey())
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){

                log.error("新增采购发票失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","savePurchaseInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_ADD_FAILED);
            }
            // 修改订单开票状态
            if (purchaseOrderMapper.update(new ErpPurchaseOrder()
                    .setInvoiceState(Status.TRUE.getKey())
                    .setUpdateTime(new Date()),new EntityWrapper<ErpPurchaseOrder>()
                    .eq("id",rq.getPurchaseOrder()).and().eq("deleted",Status.FALSE.getKey())) <= ZERO){
                log.error("修改订单开票状态失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","savePurchaseInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_INVOICE_STATE_UPDATE_FAILED);
            }
        }else {
            if (purchaseInvoiceOrderMapper.updateById(invoiceOrder
                    .setId(rq.getId())
                    .setCreateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO) {
                log.error("修改采购发票失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","savePurchaseInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_INVOICE_UPDATE_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,invoiceOrder.getId());
    }

    /**
     *
     * @param orderId
     */
    private void updatePurchaseOrderState(Integer orderId) {
        //更新发票状态
        List<ErpPurchaseInvoiceOrder> purchaseInvoiceOrders = purchaseInvoiceOrderMapper
                .selectList(new EntityWrapper<>(new ErpPurchaseInvoiceOrder()
                        .setPurchaseOrder(orderId)
                        .setDeleted(Status.FALSE.getKey())));
        int invoceState = EnumInvoiceStatus.INVOICE_NOT.getKey();
        if (CollectionUtils.isNotEmpty(purchaseInvoiceOrders)) {
            invoceState = EnumInvoiceStatus.INVOICE_ALREADY.getKey();
        }
        if (purchaseOrderMapper.updateById(new ErpPurchaseOrder().setId(orderId).setInvoiceState(invoceState)) < ZERO){
            log.error("修改订单开票状态失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","savePurchaseInvoiceOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_INVOICE_STATE_UPDATE_FAILED);
        }

    }
}

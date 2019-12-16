package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.datadriver.dao.mapper.ErpOperationLogMapper;
import com.bee.platform.datadriver.dao.mapper.ErpSaleInvoiceOrderDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpSaleInvoiceOrderMapper;
import com.bee.platform.datadriver.dao.mapper.ErpSaleOrderMapper;
import com.bee.platform.datadriver.dto.ErpSaleInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.enums.EnumErpSaleOrderStatus;
import com.bee.platform.datadriver.rq.ErpSaleInvoiceOrderSaveRQ;
import com.bee.platform.datadriver.rq.InvoiceOrderQueryRQ;
import com.bee.platform.datadriver.rq.OrderDeleteRQ;
import com.bee.platform.datadriver.service.ErpSaleInvoiceOrderService;
import com.bee.platform.datadriver.service.ErpSaleOrderService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import io.swagger.models.auth.In;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 销售发票 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Slf4j
@Service
public class ErpSaleInvoiceOrderServiceImpl extends ServiceImpl<ErpSaleInvoiceOrderMapper, ErpSaleInvoiceOrder> implements ErpSaleInvoiceOrderService {

    @Autowired
    private ErpSaleInvoiceOrderMapper saleInvoiceOrderMapper;
    @Autowired
    private ErpSaleInvoiceOrderDetailMapper invoiceOrderDetailMapper;
    @Autowired
    private ErpSaleOrderService erpSaleOrderService;
    @Autowired
    private ErpSaleOrderMapper erpSaleOrderMapper;
    @Autowired
    private ErpOperationLogMapper operationLogMapper;
    private static Integer ZERO = 0;

    /**
     * 分页查询销售订单列表
     * @param pagination
     * @param rq
     * @return
     */
    @Override
    public List<ErpSaleInvoiceOrderInfoDTO> listSaleInvoiceOrder(Pagination pagination, InvoiceOrderQueryRQ rq, AuthPlatformUserInfo userInfo) {
        if (!StringUtils.isEmpty(rq.getCreateStartTime()) && DateUtils.isValidDate(rq.getCreateStartTime())){
            rq.setCreateStartTime(rq.getCreateStartTime() + " 00:00:00");
        }
        if (!StringUtils.isEmpty(rq.getCreateEndTime()) && DateUtils.isValidDate(rq.getCreateEndTime())){
            rq.setCreateEndTime(rq.getCreateEndTime() + " 23:59:59");
        }
        rq.setCompany(userInfo.getOrgId());
        List<ErpSaleInvoiceOrderInfoDTO> list =  saleInvoiceOrderMapper.selectInvoiceOrderByCondition(rq,pagination);
        return list.stream().map(o -> {
            if (!StringUtils.isEmpty(o.getBatchName()) && !StringUtils.isEmpty(o.getProductName())) {
                o.setProductName(o.getProductName() + "-" + o.getBatchName());
            }
            return o;
        }).collect(Collectors.toList());
    }

    /**
     * 获取销售发票信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpSaleInvoiceOrder> getSaleInvoiceOrder(String id) {

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,saleInvoiceOrderMapper
                .selectById(new ErpSaleInvoiceOrder().setId(Integer.valueOf(id)).setDeleted(Status.FALSE.getKey())));
    }

    /**
     * 删除销售发票信息
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<Integer> deleteSaleInvoiceOrder(AuthPlatformUserInfo userInfo, OrderDeleteRQ rq) {
        ErpSaleInvoiceOrder invoiceOrder = saleInvoiceOrderMapper.selectById(new ErpSaleInvoiceOrder()
                .setId(rq.getId()).setDeleted(Status.FALSE.getKey()));
        if (saleInvoiceOrderMapper.updateById(new ErpSaleInvoiceOrder()
                .setId(rq.getId()).setDeleted(Status.TRUE.getKey())
                .setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) <= ZERO){
            log.error("删除销售发票失败,调用{}类{}方法出错","ErpSaleInvoiceOrderServiceImpl","deleteSaleInvoiceOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_SALE_INVOICE_DELETE_FAILED);
        }
        if (invoiceOrderDetailMapper.update(new ErpSaleInvoiceOrderDetail()
                .setDeleted(Status.TRUE.getKey()),new EntityWrapper<>(new ErpSaleInvoiceOrderDetail().setOrderId(rq.getId()))) < ZERO){
            log.error("删除销售发票明细失败,调用{}类{}方法出错","ErpPurchaseInvoiceOrderServiceImpl","deletePurchaseInvoiceOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_SALE_INVOICE_DETAIL_DELETE_FAILED);
        }
        //更新销售订单发票状态
        updateSaleOrderState(invoiceOrder.getSaleOrder());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,rq.getId());
    }

    /**
     * 保存销售发票
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<Integer> saveSaleInvoiceOrder(AuthPlatformUserInfo userInfo, ErpSaleInvoiceOrderSaveRQ rq) {
        ErpSaleInvoiceOrder invoiceOrder = BeanUtils.copyProperties(rq,ErpSaleInvoiceOrder.class);
        // 新增
        if (invoiceOrder.getId() == null){
            if (!ObjectUtils.isEmpty(saleInvoiceOrderMapper.selectOne(new ErpSaleInvoiceOrder()
                    .setCode(rq.getCode())
                    .setCompany(rq.getCompany())
                    .setDeleted(Status.FALSE.getKey())))){
                log.error("发票编号重复", "ErpSaleInvoiceOrderServiceImpl", "saveSaleInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_CODE_REPEAT);
            }
            if (saleInvoiceOrderMapper.insert(invoiceOrder
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("新增销售发票失败,调用{}类{}方法出错","ErpSaleInvoiceOrderServiceImpl","saveSaleInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_SALE_INVOICE_ADD_FAILED);
            }
            // 修改订单付款状态
            if (erpSaleOrderMapper.update(new ErpSaleOrder()
                    .setInvoceState(Status.TRUE.getKey()),new EntityWrapper<ErpSaleOrder>()
                    .eq("id",rq.getSaleOrder()).and().eq("deleted",Status.FALSE.getKey())) <= ZERO){
                log.error("修改订单付款失败,调用{}类{}方法出错","ErpSaleInvoiceOrderServiceImpl","saveSaleInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_INVOICE_STATE_UPDATE_FAILED);
            }
            // 插入操作日志
            insertOperationLog(invoiceOrder,userInfo, OperateType.ADD.getMsg());
        }else {
            if (saleInvoiceOrderMapper.updateById(invoiceOrder
                    .setId(rq.getId())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO){
                log.error("修改销售发票失败,调用{}类{}方法出错","ErpSaleInvoiceOrderServiceImpl","saveSaleInvoiceOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_SALE_INVOICE_UPDATE_FAILED);
            }
            // 插入操作日志
            insertOperationLog(invoiceOrder,userInfo,OperateType.EDIT.getMsg());
        }
        //更新销售订单发票状态
        updateSaleOrderState(rq.getSaleOrder());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,invoiceOrder.getId());
    }

    /**
     *
     * @param orderId
     */
    private void updateSaleOrderState(Integer orderId) {
        //更新发票状态
        List<ErpSaleInvoiceOrder> erpSaleInvoiceOrders = saleInvoiceOrderMapper
                .selectList(new EntityWrapper<>(new ErpSaleInvoiceOrder()
                        .setSaleOrder(orderId)
                        .setDeleted(Status.FALSE.getKey())));
        int invoceState = EnumErpSaleOrderStatus.InvoiceType.N0T_INVOICE.getKey();
        if (CollectionUtils.isNotEmpty(erpSaleInvoiceOrders)) {
            invoceState = EnumErpSaleOrderStatus.InvoiceType.HAD_INVOICE.getKey();
        }
        erpSaleOrderService.updateById(new ErpSaleOrder().setId(orderId).setInvoceState(invoceState));
    }

    /**
     * 插入操作日志
     * @param invoiceOrder
     * @param userInfo
     * @param msg
     */
    private void insertOperationLog(ErpSaleInvoiceOrder invoiceOrder, AuthPlatformUserInfo userInfo, String msg) {
        // 插入操作日志
        if(operationLogMapper.insert(new ErpOperationLog()
                .setBusinessId(invoiceOrder.getId())
                .setCompanyId(userInfo.getOrgId())
                .setBusinessType(EnumBusinessType.SALE_INVOICE.getCode())
                .setOperateMsg(msg)
                .setOperator(userInfo.getId())
                .setOperatorName(userInfo.getName())
                .setOperateTime(new Date())) <= ZERO){
            log.error("新增付款单操作日志失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","saveErpPayOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_LOG_ADD_FAILED);
        }
    }
}

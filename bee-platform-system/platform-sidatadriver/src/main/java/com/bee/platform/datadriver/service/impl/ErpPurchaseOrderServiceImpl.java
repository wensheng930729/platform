package com.bee.platform.datadriver.service.impl;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.support.OperateType;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import com.alibaba.fastjson.JSONArray;
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
import com.bee.platform.datadriver.enums.EnumAccountType;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.enums.EnumPayType;
import com.bee.platform.datadriver.enums.EnumReceiveType;
import com.bee.platform.datadriver.enums.EnumSaleOrderStatus;
import com.bee.platform.datadriver.rq.OrderDeleteRQ;
import com.bee.platform.datadriver.rq.OrderQueryRQ;
import com.bee.platform.datadriver.rq.PurchaseOrderSaveRQ;
import com.bee.platform.datadriver.service.ErpPurchaseOrderService;
import com.bee.platform.datadriver.service.ErpRepoReceiptDetailService;
import com.bee.platform.datadriver.service.ErpRepositoryReceiptService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 采购订单 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpPurchaseOrderServiceImpl extends ServiceImpl<ErpPurchaseOrderMapper, ErpPurchaseOrder> implements ErpPurchaseOrderService {

    @Autowired
    private ErpPurchaseOrderMapper purchaseOrderMapper;
    @Autowired
    private ErpPurchaseOrderDetailMapper purchaseOrderDetailMapper;
    @Autowired
    private ErpPayOrderMapper payOrderMapper;
    @Autowired
    private ErpRepoReceiptDetailMapper repoReceiptDetailMapper;
    @Autowired
    private ErpPurchaseStmtDetailMapper stmtDetailMapper;
    @Autowired
    private ErpTestReportMapper testReportMapper;
    @Autowired
    private ErpPurchaseStatementMapper purchaseStatementMapper;
    @Autowired
    private ErpOperationLogMapper operationLogMapper;
    @Autowired
    private ErpProductBatchMapper productBatchMapper;

    private static Integer ZERO = 0;

    /**
     * 分页查询采购订单列表
     * @param pagination
     * @param rq
     * @return
     */
    @Override
    public List<ErpPurchaseOrderInfoDTO> listErpPurchaseOrder(Pagination pagination, OrderQueryRQ rq, AuthPlatformUserInfo userInfo) {
        if (!StringUtils.isEmpty(rq.getCreateStartTime()) && DateUtils.isValidDate(rq.getCreateStartTime())){
            rq.setCreateStartTime(rq.getCreateStartTime() + " 00:00:00");
        }
        if (!StringUtils.isEmpty(rq.getCreateEndTime()) && DateUtils.isValidDate(rq.getCreateEndTime())){
            rq.setCreateEndTime(rq.getCreateEndTime() + " 23:59:59");
        }
        rq.setCompany(userInfo.getOrgId());
        List<ErpPurchaseOrderInfoDTO> list = purchaseOrderMapper.selectPurchaseOrderByCondition(rq, pagination);
        return list.stream().map(o -> {
            if (!StringUtils.isEmpty(o.getBatchName()) && !StringUtils.isEmpty(o.getProductName())) {
                o.setProductName(o.getProductName() + "-" + o.getBatchName());
            }
            return o;
        }).collect(Collectors.toList());
    }

    /**
     * 获取采购订单信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpPurchaseOrderDTO> getErpPurchaseOrder(AuthPlatformUserInfo userInfo, Integer id) {
        // 查询订单详情
        ErpPurchaseOrderDTO erpPurchaseOrderDTO = purchaseOrderMapper.getOrderById(id);
        // 查询批次
        List<ErpProductBatchListDTO> batchs = productBatchMapper.getPurchaseBatchsByOrderId(id);
        erpPurchaseOrderDTO.setProductBatchList(batchs);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpPurchaseOrderDTO);
    }

    /**
     * 保存采购订单
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    public ResponseResult<Integer> saveErpPurchaseOrder(AuthPlatformUserInfo userInfo, PurchaseOrderSaveRQ rq) {
        ErpPurchaseOrder purchaseOrder = BeanUtils.copyProperties(rq, ErpPurchaseOrder.class);
        // 新增
        if (purchaseOrder.getId() == null) {
            if (!ObjectUtils.isEmpty(purchaseOrderMapper.selectOne(new ErpPurchaseOrder()
                    .setContractNo(rq.getContractNo())
                    .setCompany(rq.getCompany())
                    .setDeleted(Status.FALSE.getKey())))){
                log.error("采购订单合同号重复", "ErpPurchaseOrderServiceImpl", "saveErpPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_CODE_REPEAT);
            }
            if (purchaseOrderMapper.insert(purchaseOrder
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())) <= ZERO) {
                log.error("新增采购单明细失败,调用{}类{}方法出错", "ErpPurchaseOrderServiceImpl", "saveErpPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_ADD_FAILED);
            }
            // 插入操作日志
            insertOperationLog(purchaseOrder,userInfo, OperateType.ADD.getMsg());
        }else {
            if (purchaseOrderMapper.updateById(purchaseOrder
                    .setId(rq.getId())
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO) {
                log.error("修改采购单明细失败,调用{}类{}方法出错", "ErpPurchaseOrderServiceImpl", "saveErpPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_UPDATE_FAILED);
            }
            // 插入操作日志
            insertOperationLog(purchaseOrder,userInfo, OperateType.EDIT.getMsg());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,purchaseOrder.getId());
    }

    /**
     * 统计采购订单状态信息
     * @param companyId
     * @return
     */
    @Override
    public ResponseResult<OrderStatusCountDTO> countErpPurchaseOrderStatu(Integer companyId) {
        OrderStatusCountDTO countDTO = new OrderStatusCountDTO();
        // 待收货
        Integer deliveryCount = purchaseOrderMapper.selectCount(new EntityWrapper<ErpPurchaseOrder>()
                .ne("receive_state", EnumReceiveType.RECEIVED_ALL.getKey()).and()
                .eq("deleted", Status.FALSE.getKey()).and()
                .eq("company", companyId));
        // 待结算
        Integer accountCount = purchaseOrderMapper.selectCount(new EntityWrapper<ErpPurchaseOrder>()
                .eq("account_state", EnumAccountType.ACCOUNT_NOT.getKey()).and()
                .eq("deleted", Status.FALSE.getKey()).and()
                .eq("company", companyId));
        // 待付款
        Integer payCount = purchaseOrderMapper.selectCount(new EntityWrapper<ErpPurchaseOrder>()
                .eq("pay_state", EnumPayType.PAY_NOT.getKey()).and()
                .eq("deleted", Status.FALSE.getKey()).and()
                .eq("company", companyId));
        countDTO.setDeliveryCount(deliveryCount);
        countDTO.setAccountCount(accountCount);
        countDTO.setCollectionCount(payCount);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, countDTO);
    }

    /**
     * 根据合同号获取采购订单信息
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<List<ErpPurchaseOrderInfoDetailDTO>> getErpPurchaseOrderById(AuthPlatformUserInfo userInfo, Integer id) {
        List<ErpPurchaseOrderInfoDetailDTO> result = new ArrayList<>();
        //查询采购订单对应的收货信息
        List<ErpRepoReceiptDetail> purchaseGoodsList = purchaseOrderMapper.getPurchaseGoodsList(id);
        Map<Integer, BigDecimal> purchaseGoodsMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(purchaseGoodsList)) {
            Map<Integer, List<ErpRepoReceiptDetail>> goodsMap = purchaseGoodsList.stream().collect(Collectors.groupingBy(ErpRepoReceiptDetail :: getProductBatchId));
            for (Map.Entry<Integer, List<ErpRepoReceiptDetail>> entry : goodsMap.entrySet()) {
                List<ErpRepoReceiptDetail> goods = entry.getValue();
                if (CollectionUtils.isEmpty(goods)) {
                    continue;
                }
                //获取采购订单对应的采购收货数量合计
                BigDecimal totalNum = goods.stream().map(ErpRepoReceiptDetail::getNum)
                        .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                purchaseGoodsMap.put(entry.getKey(), totalNum);
            }
        }

        //查询采购订单详情列表
        List<ErpPurchaseOrderInfoDetailDTO> purchaseOrders = purchaseOrderMapper.findByOrderId(id);
        if (CollectionUtils.isNotEmpty(purchaseOrders)) {
            Map<Integer, List<ErpPurchaseOrderInfoDetailDTO>> orderMap = purchaseOrders.stream().collect(Collectors.groupingBy(ErpPurchaseOrderInfoDetailDTO :: getProductBatchId));
            for (Map.Entry<Integer, List<ErpPurchaseOrderInfoDetailDTO>> entry : orderMap.entrySet()) {
                List<ErpPurchaseOrderInfoDetailDTO> orders = entry.getValue();
                if (CollectionUtils.isEmpty(orders)) {
                    continue;
                }
                //总数量
                BigDecimal totalNum = BigDecimal.ZERO;
                //总价格=所有单价*数量之和
                BigDecimal totalAmount = BigDecimal.ZERO;
                for (ErpPurchaseOrderInfoDetailDTO order : orders) {
                    if (Objects.isNull(order.getNum()) || Objects.isNull(order.getTaxPrice())) {
                        continue;
                    }
                    totalNum = totalNum.add(order.getNum());
                    totalAmount = totalAmount.add(order.getTaxPrice().multiply(order.getNum()));
                }
                //计算出单价
                BigDecimal taxPrice = BigDecimal.ZERO;
                if (totalNum.compareTo(BigDecimal.ZERO) == 1) {
                    taxPrice = totalAmount.divide(totalNum, 2, BigDecimal.ROUND_HALF_UP);
                }
                ErpPurchaseOrderInfoDetailDTO detailDTO = BeanUtils.copyProperties(orders.get(0), ErpPurchaseOrderInfoDetailDTO.class);
                if (StringUtils.isNotBlank(detailDTO.getProductName()) && StringUtils.isNotBlank(detailDTO.getBatchName())) {
                    detailDTO.setProductName(detailDTO.getProductName() + "-" + detailDTO.getBatchName());
                }
                detailDTO.setNum(totalNum).setTaxPrice(taxPrice).setTotalNum(purchaseGoodsMap.get(entry.getKey()));
                result.add(detailDTO);
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * 	根据当前用户查询，当前用户公司及其子公司的采购订单合同编号
     *
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<ErpPucharseOrderBoxDTO>> getPucharseOrderBox(AuthPlatformUserInfo userInfo) {
        List<ErpPurchaseOrder> purchaseOrders = selectList(new EntityWrapper<>(new ErpPurchaseOrder()
                .setDeleted(Status.FALSE.getKey()).setCompany(userInfo.getOrgId())));
        List<ErpPucharseOrderBoxDTO> boxDTOS = BeanUtils.assemble(ErpPucharseOrderBoxDTO.class, purchaseOrders);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOS);
    }

    /**
     * 删除采购订单信息
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteErpPurchaseOrder(AuthPlatformUserInfo userInfo, Integer id) {
        if (purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())) < ZERO){
            log.error("删除采购单失败,调用{}类{}方法出错","ErpPurchaseOrderServiceImpl","deleteErpPurchaseOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DELETE_FAILED);
        }
        if (purchaseOrderDetailMapper.update(new ErpPurchaseOrderDetail()
                .setDeleted(Status.TRUE.getKey()),new EntityWrapper<>(new ErpPurchaseOrderDetail().setOrderId(id))) < ZERO){
            log.error("删除采购单明细失败,调用{}类{}方法出错","ErpPurchaseOrderServiceImpl","deleteErpPurchaseOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }

    /**
     * 订单执行情况
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpPurchaseOrderExcuteDTO> getErpPurchaseOrderExcute(Integer id) {
        ErpPurchaseOrderExcuteDTO excuteDTO = new ErpPurchaseOrderExcuteDTO();
        BigDecimal receiveSum = BigDecimal.valueOf(0);
        BigDecimal receiveTotal = BigDecimal.valueOf(0);
        BigDecimal receivedGrade = BigDecimal.valueOf(0);
        BigDecimal accountSum = BigDecimal.valueOf(0);
        BigDecimal accountTotal = BigDecimal.valueOf(0);
        BigDecimal receiveWeight = BigDecimal.valueOf(0);
        BigDecimal accountGrade = BigDecimal.valueOf(0);
        BigDecimal accountWeight = BigDecimal.valueOf(0);
        BigDecimal payAmount = BigDecimal.valueOf(0);
        List<ErpRepoReceiptDetail> repoReceiptDetails = repoReceiptDetailMapper.listByOrderId(EnumBusinessType.MATERIAL_STOCK.getCode(), id);
        if (!CollectionUtils.isEmpty(repoReceiptDetails)){
            receiveWeight = repoReceiptDetails.stream().map(ErpRepoReceiptDetail::getNum)
                    .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            excuteDTO.setReceivedWeight(receiveWeight);


            for (ErpRepoReceiptDetail e: repoReceiptDetails) {
                ErpTestReport testReport = testReportMapper.selectOne(new ErpTestReport()
                        .setId(e.getTestId()).setDeleted(Status.FALSE.getKey()));
                if (ObjectUtils.isEmpty(testReport)){
                    continue;
                }
                // 化验项
                List<ErpTestReportDetailDTO> detailDTO = (List<ErpTestReportDetailDTO>) JSONArray
                        .parseArray(testReport.getResult(), ErpTestReportDetailDTO.class);
                for (ErpTestReportDetailDTO dto : detailDTO) {
                    if ("Cr2O3".equalsIgnoreCase(dto.getTestItem())){
                        receiveTotal =  receiveTotal.add((StringUtils.isEmpty(dto.getTestValue()) ? BigDecimal.ZERO : new BigDecimal(dto.getTestValue())).multiply(e.getNum()));
                    }
                }
                receiveSum = receiveSum.add(e.getNum());
            }
        }
        // 收货品位
        if (receiveSum.compareTo(BigDecimal.valueOf(0)) == 0){
            receivedGrade = BigDecimal.valueOf(0);
        }else {
            receivedGrade = receiveTotal.divide(receiveSum, 2, BigDecimal.ROUND_HALF_UP);
        }
        excuteDTO.setReceivedWeight(receiveWeight);
        excuteDTO.setReceivedGrade(receivedGrade);
        // 结算
        List<ErpPurchaseStmtDetailDTO> purchaseStatements = stmtDetailMapper.listPurchaseStmtDetailByOrder(id.toString());
        if (!CollectionUtils.isEmpty(purchaseStatements)){
            for (ErpPurchaseStmtDetailDTO e: purchaseStatements) {
                if (e.getGrade() != null){
                    accountTotal =  accountTotal.add(e.getGrade().multiply(e.getSrcDryWeight()));
                }
                accountSum = accountSum.add(e.getSrcDryWeight());
            }
            if (accountSum.compareTo(BigDecimal.valueOf(0)) == 0){
                accountGrade = BigDecimal.valueOf(0);
            }else {
                accountGrade = accountTotal.divide(accountSum, 2, BigDecimal.ROUND_HALF_UP);
            }

            // 结算重量
            accountWeight = purchaseStatements.stream().map(ErpPurchaseStmtDetailDTO::getRealDryWeight)
                    .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            excuteDTO.setAccountWeight(accountWeight);
        }
        excuteDTO.setAccountGrade(accountGrade);
        excuteDTO.setAccountWeight(accountWeight);
        // 干吨盈亏
        excuteDTO.setDryBalance(receiveWeight.subtract(accountWeight));
        // 品位盈亏
        excuteDTO.setGradeBalance(receivedGrade.subtract(accountGrade));
        // 已付金额
        List<ErpPayOrder> payOrders = payOrderMapper.selectList(new EntityWrapper<>(new ErpPayOrder()
                .setPurchaseOrder(id).setDeleted(Status.FALSE.getKey())));
        if (!CollectionUtils.isEmpty(payOrders)){
            payAmount = payOrders.stream().map(ErpPayOrder::getAmount)
                    .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        }
        excuteDTO.setPayAmount(payAmount);
        // 执行状态
        excuteDTO.setExcuteStatu(this.getOrderExcuteStatu(id));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,excuteDTO);
    }

    /**
     * 订单执行订单执行状态
     * @param id
     * @return
     */
    @Override
    public Integer getOrderExcuteStatu(Integer id) {
        ErpPurchaseOrder purchaseOrder = purchaseOrderMapper.selectById(id);
        // 是否付款
        if (purchaseOrder.getPayState() > 0){
            return EnumSaleOrderStatus.RECEIVED.getKey();
        }
        // 是否结算
        List<ErpPurchaseStatement> statements = purchaseStatementMapper.selectList(new EntityWrapper<>(new ErpPurchaseStatement()
                .setOrderId(id).setDeleted(Status.FALSE.getKey())));
        if (!CollectionUtils.isEmpty(statements)){
            return EnumSaleOrderStatus.SETTLED.getKey();
        }
        //若未结算，查询是否收货完成
        //合同数量
        List<ErpPurchaseOrderDetail> purchaseOrderDetails = purchaseOrderDetailMapper
                .selectList(new EntityWrapper<>(new ErpPurchaseOrderDetail()
                        .setOrderId(id).setDeleted(Status.FALSE.getKey())));
        if (CollectionUtils.isNotEmpty(purchaseOrderDetails)) {
            BigDecimal totalNum = purchaseOrderDetails.stream().map(ErpPurchaseOrderDetail::getNum)
                    .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            //收货数量
            List<ErpRepoReceiptDetail> repoReceiptDetails = repoReceiptDetailMapper
                    .listByOrderId(EnumBusinessType.MATERIAL_STOCK.getCode(), id);
            if (CollectionUtils.isNotEmpty(repoReceiptDetails)) {
                BigDecimal deliverylNum = repoReceiptDetails.stream().map(ErpRepoReceiptDetail::getNum)
                        .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                //若合同数量95%<发货数量<合同数量105%
                BigDecimal a = totalNum.multiply(BigDecimal.valueOf(0.95));
                BigDecimal b = totalNum.multiply(BigDecimal.valueOf(1.05));
                if (deliverylNum.compareTo(a) == 1 && deliverylNum.compareTo(b) == -1) {
                    return EnumSaleOrderStatus.DELIVERY_COMPLETED.getKey();
                } else if (deliverylNum.compareTo(BigDecimal.ZERO) == 1){
                    return EnumSaleOrderStatus.EXECUTION.getKey();
                }
            }

        }
        return EnumSaleOrderStatus.SIGNED.getKey();
    }

    /**
     * 插入操作日志
     * @param purchaseOrder
     * @param userInfo
     * @param msg
     */
    private void insertOperationLog(ErpPurchaseOrder purchaseOrder, AuthPlatformUserInfo userInfo, String msg) {
        // 插入操作日志
        if(operationLogMapper.insert(new ErpOperationLog()
                .setBusinessId(purchaseOrder.getId())
                .setCompanyId(userInfo.getOrgId())
                .setBusinessType(EnumBusinessType.PURCHASE.getCode())
                .setOperateMsg(msg)
                .setOperator(userInfo.getId())
                .setOperatorName(userInfo.getName())
                .setOperateTime(new Date())) <= ZERO){
            log.error("新增付款单操作日志失败,调用{}类{}方法出错","ErpPayOrderServiceImpl","saveErpPayOrder()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_TEST_REPORT_LOG_ADD_FAILED);
        }
    }
}

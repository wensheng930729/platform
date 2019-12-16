package com.bee.platform.datadriver.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

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
import com.bee.platform.datadriver.dao.mapper.ErpPayOrderMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseStatementMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseStmtDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpRepoReceiptDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpTestReportMapper;
import com.bee.platform.datadriver.dto.ErpPucharseOrderBoxDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderExcuteDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderInfoDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderInfoDetailDTO;
import com.bee.platform.datadriver.dto.ErpTestReportDetailDTO;
import com.bee.platform.datadriver.dto.OrderStatusCountDTO;
import com.bee.platform.datadriver.entity.ErpPayOrder;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.entity.ErpPurchaseOrderDetail;
import com.bee.platform.datadriver.entity.ErpPurchaseStatement;
import com.bee.platform.datadriver.entity.ErpPurchaseStmtDetail;
import com.bee.platform.datadriver.entity.ErpRepoReceiptDetail;
import com.bee.platform.datadriver.entity.ErpTestReport;
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
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
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
    private ErpRepositoryReceiptService repositoryReceiptService;

    @Autowired
    private ErpRepoReceiptDetailService receiptDetailService;

    private static Integer ZERO = 0;

    /**
     * 分页查询采购订单列表
     * @param pagination
     * @param rq
     * @return
     */
    @Override
    public List<ErpPurchaseOrderInfoDTO> listErpPurchaseOrder(Pagination pagination, OrderQueryRQ rq, Integer companyId) {
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (CollectionUtils.isEmpty(enterpriseFlatDTOS)){
            log.error("用户不在任何企业下", "ErpPurchaseOrderServiceImpl", "listErpPurchaseOrder()");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if (!StringUtils.isEmpty(rq.getCreateStartTime()) && DateUtils.isValidDate(rq.getCreateStartTime())){
            rq.setCreateStartTime(rq.getCreateStartTime() + " 00:00:00");
        }
        if (!StringUtils.isEmpty(rq.getCreateEndTime()) && DateUtils.isValidDate(rq.getCreateEndTime())){
            rq.setCreateEndTime(rq.getCreateEndTime() + " 23:59:59");
        }
        rq.setList(enterpriseIds);
        return purchaseOrderMapper.selectPurchaseOrderByCondition(rq, pagination);
    }

    /**
     * 获取采购订单信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpPurchaseOrderDTO> getErpPurchaseOrder(AuthPlatformUserInfo userInfo, String id) {
        ErpPurchaseOrderDTO erpPurchaseOrderDTO = purchaseOrderMapper.getOrderById(id);
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
        // TODO
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
            // 修改
        } else {
            if (purchaseOrderMapper.updateById(purchaseOrder
                    .setId(rq.getId())
                    .setDeleted(Status.FALSE.getKey())
                    .setCreateUser(userInfo.getId())
                    .setUpdateTime(new Date())) <= ZERO) {
                log.error("修改采购单明细失败,调用{}类{}方法出错", "ErpPurchaseOrderServiceImpl", "saveErpPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_UPDATE_FAILED);
            }

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
        //查询当前登录人所在的企业及子企业
        /*List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
                .getEnterpriseFlatByUser(userInfo.getSysToken()).getObject();*/
        List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterprises)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpPurchaseOrderServiceImpl", "countErpPurchaseOrderStatu");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        List<Integer> enterpriseIds = new ArrayList<Integer>();
        if (CollectionUtils.isNotEmpty(enterprises)) {
            enterprises.forEach(enterprise -> {
                enterpriseIds.add(enterprise.getValue());
            });
        }
        OrderStatusCountDTO countDTO = new OrderStatusCountDTO();
        // 待收货
        Integer deliveryCount = purchaseOrderMapper.selectCount(new EntityWrapper<ErpPurchaseOrder>()
                .ne("receive_state", EnumReceiveType.RECEIVED_ALL.getKey()).and()
                .eq("deleted", Status.FALSE.getKey()).and().in("company", enterpriseIds));
        // 待结算
        Integer accountCount = purchaseOrderMapper.selectCount(new EntityWrapper<ErpPurchaseOrder>()
                .eq("account_state", EnumAccountType.ACCOUNT_NOT.getKey()).and()
                .eq("deleted", Status.FALSE.getKey()).and().in("company", enterpriseIds));
        // 待付款
        Integer payCount = purchaseOrderMapper.selectCount(new EntityWrapper<ErpPurchaseOrder>()
                .eq("pay_state", EnumPayType.PAY_NOT.getKey()).and()
                .eq("deleted", Status.FALSE.getKey()).and().in("company", enterpriseIds));
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
    public ResponseResult<ErpPurchaseOrderInfoDetailDTO> getErpPurchaseOrderById(AuthPlatformUserInfo userInfo, Integer id) {
        ErpPurchaseOrderInfoDetailDTO detailDTO = null;
        List<ErpPurchaseOrderInfoDetailDTO> list = purchaseOrderMapper.selectByOrderId(id);
        if (CollectionUtils.isNotEmpty(list)) {
            detailDTO = BeanUtils.copyProperties(list.get(ZERO), ErpPurchaseOrderInfoDetailDTO.class);
            detailDTO.setCompany(list.get(ZERO).getCompany());
            BigDecimal numTotal = list.stream().map(ErpPurchaseOrderInfoDetailDTO::getNum).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            BigDecimal totalPrice = BigDecimal.ZERO;
            for (ErpPurchaseOrderInfoDetailDTO po : list) {
                totalPrice = totalPrice.add(po.getTaxPrice().multiply(po.getNum()));
            }
            BigDecimal taxPrice = BigDecimal.ZERO;
            if (numTotal.compareTo(BigDecimal.ZERO) == 1) {
                taxPrice = totalPrice.divide(numTotal, 2, BigDecimal.ROUND_HALF_UP);
            }
            detailDTO.setNum(numTotal);
            detailDTO.setTaxPrice(taxPrice);
            List<ErpRepoReceiptDetail> purchaseGoodsList = purchaseOrderMapper.getPurchaseGoodsList(id);
            if (CollectionUtils.isNotEmpty(purchaseGoodsList)) {
                //获取采购订单对应的采购收货的湿重合计
                BigDecimal totalWetWeight = purchaseGoodsList.stream().map(ErpRepoReceiptDetail::getWetWeight)
                        .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                detailDTO.setTotalWetWeight(totalWetWeight);
                //获取采购订单对应的采购收货的水分率加权合计
                BigDecimal totalWaterRate = BigDecimal.ZERO;
                for (ErpRepoReceiptDetail repoReceiptDetail : purchaseGoodsList) {
                    if (StringUtils.isBlank(repoReceiptDetail.getWaterRate())) {
                        continue;
                    }
                    BigDecimal waterRate = new BigDecimal(repoReceiptDetail.getWaterRate());
                    if (waterRate.compareTo(BigDecimal.ZERO) == 1) {
                        totalWaterRate = totalWaterRate.add(repoReceiptDetail.getWetWeight()
                                .multiply(waterRate));
                    }

                }
                totalWaterRate = totalWaterRate.divide(new BigDecimal(100), 2, BigDecimal.ROUND_HALF_UP);
                detailDTO.setTotalWaterRate(totalWaterRate);
                //水分率
                BigDecimal waterRate = BigDecimal.ZERO;
                if (totalWetWeight.compareTo(BigDecimal.ZERO) == 1) {
                    waterRate = totalWaterRate.divide(totalWetWeight, 2, BigDecimal.ROUND_HALF_UP);
                }
                detailDTO.setWaterRate(waterRate);
                //获取采购订单对应的采购收货数量合计
                BigDecimal totalNum = purchaseGoodsList.stream().map(ErpRepoReceiptDetail::getNum)
                        .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                detailDTO.setTotalNum(totalNum);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    /**
     * 	根据当前用户查询，当前用户公司及其子公司的采购订单合同编号
     *
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<ErpPucharseOrderBoxDTO>> getPucharseOrderBox(AuthPlatformUserInfo userInfo,String sysToken) {
        if (Objects.isNull(userInfo.getOrgId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_JOIN_ENTERPRISE);
        }
        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpPurchaseOrderServiceImpl", "getPucharseOrderBox");
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList());
        }
        List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(enterpriseIds)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_JOIN_ENTERPRISE);
        }
        List<ErpPurchaseOrder> purchaseOrders = selectList(new EntityWrapper<>(new ErpPurchaseOrder()
                .setDeleted(Status.FALSE.getKey())).in("company", enterpriseIds));
        List<ErpPucharseOrderBoxDTO> boxDTOS = BeanUtils.assemble(ErpPucharseOrderBoxDTO.class, purchaseOrders);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOS);
    }

    /**
     * 删除采购订单信息
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteErpPurchaseOrder(AuthPlatformUserInfo userInfo, OrderDeleteRQ rq) {
        /*if (rq.getSubId() != null){
            ErpPurchaseOrderDetail detail = purchaseOrderDetailMapper.selectById(new ErpPurchaseOrderDetail()
                    .setId(Integer.valueOf(rq.getSubId())).setDeleted(Status.FALSE.getKey()));
            if (purchaseOrderDetailMapper.updateById(new ErpPurchaseOrderDetail()
                    .setId(Integer.valueOf(rq.getSubId())).setDeleted(Status.TRUE.getKey())) <= ZERO){
                log.error("删除采购单明细失败,调用{}类{}方法出错","ErpPurchaseOrderServiceImpl","deleteErpPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
            if (purchaseOrderDetailMapper.selectCount(new EntityWrapper<>(new ErpPurchaseOrderDetail()
                    .setOrderId(detail.getOrderId()).setDeleted(Status.FALSE.getKey()))) <= ZERO){
                if (purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                        .setId(Integer.valueOf(detail.getOrderId()))
                        .setDeleted(Status.TRUE.getKey())
                        .setUpdateTime(new Date())) <= ZERO){
                    log.error("删除采购单失败,调用{}类{}方法出错","ErpPurchaseOrderServiceImpl","deleteErpPurchaseOrder()");
                    throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DELETE_FAILED);
                }
            }
        }else {
            if (purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                    .setId(Integer.valueOf(rq.getId()))
                    .setDeleted(Status.TRUE.getKey())
                    .setUpdateTime(new Date())) <= ZERO){
                log.error("删除采购单失败,调用{}类{}方法出错","ErpPurchaseOrderServiceImpl","deleteErpPurchaseOrder()");
                throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DELETE_FAILED);
            }
        }*/
        if (purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                .setId(rq.getId())
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())) < ZERO){
            log.error("删除采购单失败,调用{}类{}方法出错","ErpPurchaseOrderServiceImpl","deleteErpPurchaseOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_PURCHASE_DELETE_FAILED);
        }
        if (purchaseOrderDetailMapper.update(new ErpPurchaseOrderDetail()
                .setDeleted(Status.TRUE.getKey()),new EntityWrapper<>(new ErpPurchaseOrderDetail().setOrderId(rq.getId()))) < ZERO){
            log.error("删除采购单明细失败,调用{}类{}方法出错","ErpPurchaseOrderServiceImpl","deleteErpPurchaseOrder()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,rq.getId());
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
        List<ErpPurchaseStmtDetail> purchaseStatements = stmtDetailMapper.listPurchaseStmtDetailByOrder(id.toString());
        if (!CollectionUtils.isEmpty(purchaseStatements)){
            for (ErpPurchaseStmtDetail e: purchaseStatements) {
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
            accountWeight = purchaseStatements.stream().map(ErpPurchaseStmtDetail::getRealDryWeight)
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
}

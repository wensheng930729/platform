package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.customer.dto.ErpSaleOrderDetailDTO;
import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.enums.EnumAccountType;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.enums.EnumReceiveType;
import com.bee.platform.datadriver.enums.EnumSaleOrderStatus;
import com.bee.platform.datadriver.rq.ErpSaleOrderListRQ;
import com.bee.platform.datadriver.rq.ErpSaleOrderQueryRQ;
import com.bee.platform.datadriver.rq.ErpSaleOrderUpdateRQ;
import com.bee.platform.datadriver.service.ErpOperationLogService;
import com.bee.platform.datadriver.service.ErpSaleOrderService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.authority.dto.AuthPlatformUserFeignDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 销售单 服务实现类
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpSaleOrderServiceImpl extends ServiceImpl<ErpSaleOrderMapper, ErpSaleOrder> implements ErpSaleOrderService {

    @Autowired
    private ErpSaleOrderMapper erpSaleOrderMapper;
    @Autowired
    private AuthEnterpriseFeignClient enterpriseFeignClient;
    @Autowired
    private ErpSaleOrderDetailMapper erpSaleOrderDetailMapper;
    @Autowired
    private ErpProductMapper erpProductMapper;
    @Autowired
    private ErpRepoReceiptDetailMapper repoReceiptDetailMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpSaleOrderService saleOrderService;
    @Autowired
    private ErpReceiptOrderDetailMapper receiptOrderDetailMapper;
    @Autowired
    private ErpTestReportMapper testReportMapper;
    @Autowired
    private ErpSaleStmtDetailMapper saleStmtDetailMapper;
    @Autowired
    private ErpSaleStatementMapper saleStatementMapper;
    @Autowired
    private AuthUserFeignClient authUserFeignClient;
    @Autowired
    private ErpOperationLogService operationLogService;

    /**
     * 查询销售订单
     *
     * @param pagination
     * @return
     */
    @Override
    public ResponseResult<List<ErpSaleOrderQueryDTO>> query(Integer companyId ,
                                                            ErpSaleOrderQueryRQ erpSaleOrderQueryRQ,
                                                            Pagination pagination) {
        if (Objects.isNull(erpSaleOrderQueryRQ.getCompany())) {
            List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
                    .getEnterpriseFlatByCompanyId(companyId).getObject();
            if (CollectionUtils.isNotEmpty(enterprises)) {
                List<Integer> enterpriseIds = new ArrayList<Integer>();
                enterprises.forEach(enterprise -> {
                    enterpriseIds.add(enterprise.getValue());
                });
                erpSaleOrderQueryRQ.setEnterpriseIdList(enterpriseIds);
            }
        }
        List<ErpSaleOrderQueryDTO> orderQueryDTOList = erpSaleOrderMapper.query(erpSaleOrderQueryRQ, pagination);

        //查询销售人员姓名
        if (CollectionUtils.isNotEmpty(orderQueryDTOList)){
            List<Integer> saleUserIds = new ArrayList<>(orderQueryDTOList.size());
            orderQueryDTOList.forEach(order -> {
                if (Objects.nonNull(order.getSaleUserId())) {
                    saleUserIds.add(order.getSaleUserId());
                }
            });
            if (CollectionUtils.isNotEmpty(saleUserIds)) {
                List<AuthPlatformUserFeignDTO> userList = authUserFeignClient.getMoreUserInfo(saleUserIds).getObject();
                if (CollectionUtils.isNotEmpty(userList)) {
                    Map<Integer, String> userMap = userList.stream().collect(Collectors
                            .toMap(AuthPlatformUserFeignDTO::getId, AuthPlatformUserFeignDTO::getName));
                    //设置销售人员姓名
                    orderQueryDTOList.forEach(order -> {
                        order.setSaleUserName(userMap.get(order.getSaleUserId()));
                    });
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, orderQueryDTOList, PageUtils.transToPage(pagination));

    }

    /**
     * 编辑要返回的数据
     *
     * @param id
     * @return
     */
    @Override
    public ResponseResult get(int id) {
        ErpSaleOrder erpSaleOrder = selectOne(new EntityWrapper<>(new ErpSaleOrder().setId(id).setDeleted(Status.FALSE.getKey())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpSaleOrder);

    }

    /**
     * 根据id删除
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    @Log(businessType=EnumBusinessType.SALE, operateType=OperateType.DELETE)
    public ResponseResult<Integer> delete(Integer id) {
        ErpSaleOrder erpSaleOrder = selectOne(new EntityWrapper<>(new ErpSaleOrder()
                .setId(id).setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(erpSaleOrder)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //删除主表数据
        erpSaleOrderMapper.updateById(new ErpSaleOrder()
                .setId(id)
                .setDeleted(Status.TRUE.getKey()));
        //删除明细表数据
        erpSaleOrderDetailMapper.update(new ErpSaleOrderDetail().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpSaleOrderDetail().setOrderId(id)));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }

    /**
     * 修改销售单
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    @Log(businessType=EnumBusinessType.SALE, operateType=OperateType.EDIT)
    public ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, ErpSaleOrderUpdateRQ rq) {
        ErpSaleOrder saleOrder = selectOne(new EntityWrapper<>(new ErpSaleOrder()
                .setCompany(rq.getCompany())
                .setContractNo(rq.getContractNo())
                .setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(rq.getId())) {
            //新增销售订单
            //校验订单号
            if (Objects.nonNull(saleOrder)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.SALE_ORDER_CODE_EXIST);
            }

            ErpSaleOrder erpSaleOrder = BeanUtils.copyProperties(rq, ErpSaleOrder.class);
            if (erpSaleOrderMapper.insert(erpSaleOrder
                    .setCreateUser(userInfo.getId())
                    .setCreateTime(new Date())
                    .setDeleted(Status.FALSE.getKey())) < 0){
                return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
            }
            //添加操作日志
            operationLogService.saveLog(rq.getCompany(), userInfo, erpSaleOrder.getId(), EnumBusinessType.SALE.getCode(), OperateType.ADD.getMsg());
        } else {
            //编辑销售订单
            ErpSaleOrder erpSaleOrder = erpSaleOrderMapper.selectOne(new ErpSaleOrder()
                    .setId(rq.getId()).setDeleted(Status.FALSE.getKey()));
            if (Objects.isNull(erpSaleOrder)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }
            //校验订单号
            if (Objects.nonNull(saleOrder) && !saleOrder.getId().equals(rq.getId())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.SALE_ORDER_CODE_EXIST);
            }
            BeanUtils.copyProperties(rq, erpSaleOrder);
            erpSaleOrder.setCreateTime(new Date())
                    .setCreateUser(userInfo.getId());
            if (erpSaleOrderMapper.updateById(erpSaleOrder) != 1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
            }
            //添加操作日志
            operationLogService.saveLog(rq.getCompany(), userInfo, rq.getId(), EnumBusinessType.SALE.getCode(), OperateType.EDIT.getMsg());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);

    }

    /**
     * 根据当前用户查询，当前用户公司及其子公司的销售订单合同编号
     *
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<ErpSaleOrderBoxDTO>> getSaleOrderBox(AuthPlatformUserInfo userInfo, String sysToken) {
        if (Objects.isNull(userInfo.getOrgId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_JOIN_ENTERPRISE);
        }
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
//        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpSaleOrderServiceImpl", "getSaleOrderBox");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList());
//        }
//        List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//        if (CollectionUtils.isEmpty(enterpriseIds)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_JOIN_ENTERPRISE);
//        }
//        List<ErpSaleOrder> saleOrders = selectList(new EntityWrapper<>(new ErpSaleOrder()
//                .setDeleted(Status.FALSE.getKey())).in("company", enterpriseIds));
        List<ErpSaleOrder> saleOrders = selectList(new EntityWrapper<>(new ErpSaleOrder()
                .setCompany(userInfo.getOrgId()).setDeleted(Status.FALSE.getKey())));
        List<ErpSaleOrderBoxDTO> boxDTOS = BeanUtils.assemble(ErpSaleOrderBoxDTO.class, saleOrders);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, boxDTOS);
    }

    /**
     * @Description 根据合同id查询销售订单详情
     * @Param id
     * @Date 2019/6/2 17:39
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ErpSaleOrderInfoDTO> getSaleOrderInfo(Integer id) {
        List<ErpSaleOrderQueryDTO> saleOrderInfoList = erpSaleOrderDetailMapper.findSaleOrderInfo(id);
        if (CollectionUtils.isEmpty(saleOrderInfoList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, null);
        }

        ErpSaleOrderQueryDTO saleOrderQueryDTO = null;
        List<ErpSaleOrderQueryDTO> saleInfoList = new ArrayList<>();
        List<ErpProductBatchListDTO> productBatchList = new ArrayList<>();
        Map<Integer, List<ErpSaleOrderQueryDTO>> saleOrderMap = saleOrderInfoList.stream().collect(Collectors.groupingBy(ErpSaleOrderQueryDTO :: getProductBatchId));
        for (Map.Entry<Integer, List<ErpSaleOrderQueryDTO>> entry : saleOrderMap.entrySet()) {
            List<ErpSaleOrderQueryDTO> saleOrders = entry.getValue();
            if (CollectionUtils.isEmpty(saleOrders)) {
                continue;
            }
            //总数量
            BigDecimal totalNum = BigDecimal.ZERO;
            //总价格=所有单价*数量之和
            BigDecimal totalAmount = BigDecimal.ZERO;
            for (ErpSaleOrderQueryDTO saleOrderDetail : saleOrders) {
                if (Objects.isNull(saleOrderDetail.getNum()) || Objects.isNull(saleOrderDetail.getTaxPrice())) {
                    continue;
                }
                totalNum = totalNum.add(saleOrderDetail.getNum());
                totalAmount = totalAmount.add(saleOrderDetail.getTaxPrice().multiply(saleOrderDetail.getNum()));
            }
            //计算出单价
            BigDecimal taxPrice = BigDecimal.ZERO;
            if (totalNum.compareTo(BigDecimal.ZERO) == 1) {
                taxPrice = totalAmount.divide(totalNum, 2, BigDecimal.ROUND_HALF_UP);
            }
            saleOrderQueryDTO = new ErpSaleOrderQueryDTO();
            ErpSaleOrderQueryDTO orderQueryDTO = saleOrders.get(0);
            saleOrderQueryDTO.setNum(totalNum)
                    .setOrderId(id)
                    .setContractNo(orderQueryDTO.getContractNo())
                    .setUnit(orderQueryDTO.getUnit())
                    .setTaxPrice(taxPrice)
                    .setCompany(orderQueryDTO.getCompany())
                    .setCompanyName(orderQueryDTO.getCompanyName())
                    .setProductId(orderQueryDTO.getProductId())
                    .setProductName(orderQueryDTO.getProductName())
                    .setProductBatchId(orderQueryDTO.getProductBatchId())
                    .setBatchName(orderQueryDTO.getBatchName())
                    .setSellMethod(orderQueryDTO.getSellMethod())
                    .setCustomer(orderQueryDTO.getCustomer())
                    .setCustomerName(orderQueryDTO.getCustomerName())
                    .setContractDate(orderQueryDTO.getContractDate())
                    .setContractQualityRequirements(orderQueryDTO.getContractQualityRequirements());
            //处理产品批次
            if (StringUtils.isNotBlank(orderQueryDTO.getProductName()) && StringUtils.isNotBlank(orderQueryDTO.getBatchName())) {
                ErpProductBatchListDTO productBatch = new ErpProductBatchListDTO();
                productBatch.setProductBatchId(orderQueryDTO.getProductBatchId()).setBatchName(orderQueryDTO.getBatchName());
                productBatchList.add(productBatch);
                saleOrderQueryDTO.setProductName(orderQueryDTO.getProductName() + "-" + orderQueryDTO.getBatchName());
            }
            saleInfoList.add(saleOrderQueryDTO);
        }
        ErpSaleOrderInfoDTO result = new ErpSaleOrderInfoDTO();
        result.setProductBatchList(productBatchList).setSaleOrderList(saleInfoList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * @Description 销售订单列表
     * @Param userInfo
     * @Param saleOrderListRQ
     * @Param pagination
     * @Date 2019/6/10 15:45
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<ErpSaleOrderListDTO>> findSaleOrders(Integer companyId, ErpSaleOrderListRQ saleOrderListRQ, Pagination pagination) {
        if (Objects.isNull(saleOrderListRQ.getCompany())) {
            List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
                    .getEnterpriseFlatByCompanyId(companyId).getObject();
            if (CollectionUtils.isNotEmpty(enterprises)) {
                List<Integer> enterpriseIds = new ArrayList<Integer>();
                enterprises.forEach(enterprise -> {
                    enterpriseIds.add(enterprise.getValue());
                });
                saleOrderListRQ.setEnterpriseIdList(enterpriseIds);
            }
        }
        List<ErpSaleOrderListDTO> saleOrders = erpSaleOrderMapper.findSaleOrders(saleOrderListRQ, pagination);
        if (CollectionUtils.isNotEmpty(saleOrders)) {
            List<Integer> saleUserIds = new ArrayList<>(saleOrders.size());
            saleOrders.forEach(saleOrder -> {
                if (Objects.nonNull(saleOrder.getSaleUserId())) {
                    saleUserIds.add(saleOrder.getSaleUserId());
                }
                if (StringUtils.isNotBlank(saleOrder.getProductName())
                        && StringUtils.isNotBlank(saleOrder.getBatchName())) {
                    saleOrder.setProductName(saleOrder.getProductName() + "-" + saleOrder.getBatchName());
                }
            });

            //查询销售人员姓名
            if (CollectionUtils.isNotEmpty(saleUserIds)) {
                List<AuthPlatformUserFeignDTO> userList = authUserFeignClient.getMoreUserInfo(saleUserIds).getObject();
                if (CollectionUtils.isNotEmpty(userList)) {
                    Map<Integer, String> userMap = userList.stream().collect(Collectors
                            .toMap(AuthPlatformUserFeignDTO::getId, AuthPlatformUserFeignDTO::getName));
                    //设置销售人员姓名
                    saleOrders.forEach(order -> {
                        order.setSaleUserName(userMap.get(order.getSaleUserId()));
                    });
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrders, PageUtils.transToPage(pagination));
    }

    /**
     * @Description 统计销售订单状态列表
     * @Param userInfo
     * @Date 2019/6/10 16:28
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<OrderStatusCountDTO> countErpSaleOrderStatus(Integer companyId) {
        //查询当前登录人所选的企业及子企业
//        List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient
//                .getEnterpriseFlatByCompanyId(companyId).getObject();
//        List<Integer> enterpriseIds = new ArrayList<Integer>();
//        if (CollectionUtils.isNotEmpty(enterprises)) {
//            enterprises.forEach(enterprise -> {
//                enterpriseIds.add(enterprise.getValue());
//            });
//        }

        OrderStatusCountDTO countDTO = new OrderStatusCountDTO();
        // 待发货
         Wrapper<ErpSaleOrder> deliveryEntity = new EntityWrapper<ErpSaleOrder>()
                .eq("delivery_state", EnumReceiveType.RECEIVE_NOT.getKey())
                .and()
                .eq("company", companyId)
                .and()
                .eq("deleted", Status.FALSE.getKey());
        // 待结算
        Wrapper<ErpSaleOrder> accountEntity = new EntityWrapper<ErpSaleOrder>()
                .eq("account_state", EnumAccountType.ACCOUNT_NOT.getKey())
                .and()
                .eq("company", companyId)
                .and()
                .eq("deleted", Status.FALSE.getKey());
        // 待收款
        Wrapper<ErpSaleOrder> totalOrderEntity = new EntityWrapper<ErpSaleOrder>()
                .eq("company", companyId)
                .and()
                .eq("deleted", Status.FALSE.getKey());
//        if (CollectionUtils.isNotEmpty(enterpriseIds)) {
//            deliveryEntity.and().in("company", enterpriseIds);
//            accountEntity.and().in("company", enterpriseIds);
//            totalOrderEntity.and().in("company", enterpriseIds);
//        }

        Integer deliveryCount = erpSaleOrderMapper.selectCount(deliveryEntity);
        Integer accountCount = erpSaleOrderMapper.selectCount(accountEntity);

        Integer payCount = 0;
        //查询当前登录用户拥有权限的订单
        List<ErpSaleOrder> erpSaleOrders = erpSaleOrderMapper.selectList(totalOrderEntity);
        if (CollectionUtils.isNotEmpty(erpSaleOrders)) {
            List<Integer> orderIds = new ArrayList<>();
            erpSaleOrders.forEach(saleOrder -> {
                orderIds.add(saleOrder.getId());
            });
            List<Integer> orderLis = receiptOrderDetailMapper.findOrderIdsByOrderId(orderIds);
            if (CollectionUtils.isNotEmpty(orderIds)) {
                orderIds.removeAll(orderLis);
            }
            payCount = orderIds.size();
        }
        countDTO.setDeliveryCount(deliveryCount);
        countDTO.setAccountCount(accountCount);
        countDTO.setCollectionCount(payCount);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, countDTO);
    }

    /**
     * @Description 订单列表中根据明细id获取订单详情
     * @Param id
     * @Date 2019/6/10 18:32
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ErpSaleOrderListInfoDTO> findSaleOrderInfo(Integer id) {
        ErpSaleOrderListInfoDTO saleOrderListInfoDTO = null;
        //查询订单主表信息
        ErpSaleOrder erpSaleOrder = erpSaleOrderMapper.selectOne(new ErpSaleOrder()
                .setId(id).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(erpSaleOrder)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA, saleOrderListInfoDTO);
        }
        saleOrderListInfoDTO = BeanUtils.copyProperties(erpSaleOrder, ErpSaleOrderListInfoDTO.class);

        //查询订单明细
        List<ErpSaleOrderDetailDTO> erpSaleOrderDetailList = erpSaleOrderDetailMapper.selectErpSaleOrderDetail(id);
        if (CollectionUtils.isNotEmpty(erpSaleOrderDetailList)) {
            saleOrderListInfoDTO.setSaleOrderDetailList(erpSaleOrderDetailList);
        }
        //默认已签订
        saleOrderListInfoDTO.setState(EnumSaleOrderStatus.SIGNED.getKey());

        //查询订单是否已收款
        List<ErpReceiptOrderDetail> erpReceiptOrderDetails = receiptOrderDetailMapper
                .selectList(new EntityWrapper<>(new ErpReceiptOrderDetail()
                        .setSaleOrderId(id)
                        .setDeleted(Status.FALSE.getKey())));
        if (CollectionUtils.isNotEmpty(erpReceiptOrderDetails)) {
            saleOrderListInfoDTO.setState(EnumSaleOrderStatus.RECEIVED.getKey());
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderListInfoDTO);
        }

        //若未收款，查询是否已结算
        List<ErpSaleStatement> erpSaleStatements = saleStatementMapper
                .selectList(new EntityWrapper<>(new ErpSaleStatement()
                        .setSaleOrderId(id)
                        .setDeleted(Status.FALSE.getKey())));
        //已结算
        if (CollectionUtils.isNotEmpty(erpSaleStatements)) {
            saleOrderListInfoDTO.setState(EnumSaleOrderStatus.SETTLED.getKey());
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderListInfoDTO);
        }

        //若未结算，查询是否发货完成
        //合同数量
        List<ErpSaleOrderDetail> erpSaleOrderDetails = erpSaleOrderDetailMapper
                .selectList(new EntityWrapper<>(new ErpSaleOrderDetail()
                .setOrderId(id).setDeleted(Status.FALSE.getKey())));
        if (CollectionUtils.isNotEmpty(erpSaleOrderDetails)) {
            BigDecimal totalNum = erpSaleOrderDetails.stream().map(ErpSaleOrderDetail::getNum)
                    .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            //发货数量
            List<ErpRepoReceiptDetail> repoReceiptDetails = repoReceiptDetailMapper
                    .listByOrderId(EnumBusinessType.PRODUCT_DELIVERY.getCode(), id);
            if (CollectionUtils.isNotEmpty(repoReceiptDetails)) {
                BigDecimal deliverylNum = repoReceiptDetails.stream().map(ErpRepoReceiptDetail::getNum)
                        .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
                //若合同数量95%<发货数量<合同数量105%
                BigDecimal a = totalNum.multiply(new BigDecimal("0.95"));
                BigDecimal b = totalNum.multiply(new BigDecimal("1.05"));
                if (deliverylNum.compareTo(a) == 1 && deliverylNum.compareTo(b) == -1) {
                    saleOrderListInfoDTO.setState(EnumSaleOrderStatus.DELIVERY_COMPLETED.getKey());
                } else if (deliverylNum.compareTo(BigDecimal.ZERO) == 1){
                    saleOrderListInfoDTO.setState(EnumSaleOrderStatus.EXECUTION.getKey());
                }
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderListInfoDTO);
    }

    /**
     * @Description 查询销售订单执行情况
     * @Param id
     * @Date 2019/6/10 19:52
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ErpSaleOrderExcuteDTO> getErpSaleOrderExcute(Integer id) {
        ErpSaleOrderExcuteDTO excuteDTO = new ErpSaleOrderExcuteDTO();
        List<ErpRepoReceiptDetail> repoReceiptDetails = repoReceiptDetailMapper.listByOrderId(EnumBusinessType.PRODUCT_DELIVERY.getCode(), id);
        if (CollectionUtils.isEmpty(repoReceiptDetails)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, excuteDTO);
        }
        // 发货重量
        BigDecimal receiveWeight = repoReceiptDetails.stream().map(ErpRepoReceiptDetail::getNum)
                .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        excuteDTO.setReceivedWeight(receiveWeight);

        BigDecimal receiveSum = BigDecimal.ZERO;
        BigDecimal receiveTotal = BigDecimal.ZERO;
        for (ErpRepoReceiptDetail e: repoReceiptDetails) {
            if (Objects.isNull(e.getNum()) || Objects.isNull(e.getGrade())) {
                continue;
            }
            receiveTotal = receiveTotal.add(e.getGrade().multiply(e.getNum()));
            receiveSum = receiveSum.add(e.getNum());
        }
        BigDecimal receivedGrade = BigDecimal.ZERO;
        // 发货品位
        if (receiveSum.compareTo(BigDecimal.ZERO) == 0){
            receivedGrade = BigDecimal.ZERO;
        }else {
            receivedGrade = receiveTotal.divide(receiveSum, 2, BigDecimal.ROUND_HALF_UP);
        }
        excuteDTO.setReceivedGrade(receivedGrade);
        // 结算
        List<ErpSaleStmtDetail> saleStmtDetails = saleStmtDetailMapper.listSaleStmtDetailByOrder(id);
        BigDecimal accountSum = BigDecimal.ZERO;
        BigDecimal accountTotal = BigDecimal.ZERO;
        if (CollectionUtils.isNotEmpty(saleStmtDetails)) {
            for (ErpSaleStmtDetail s: saleStmtDetails) {
                if (Objects.isNull(s.getRealGrade()) || Objects.isNull(s.getRealNum())) {
                    continue;
                }
                accountTotal =  accountTotal.add(s.getRealGrade().multiply(s.getRealNum()));
                accountSum = accountSum.add(s.getRealNum());
            }
        }
        BigDecimal accountGrade = BigDecimal.ZERO;
        if (accountSum.compareTo(BigDecimal.ZERO) == 0){
            accountGrade = BigDecimal.ZERO;
        }else {
            accountGrade = accountTotal.divide(accountSum, 2, BigDecimal.ROUND_HALF_UP);
        }
        // 结算品位
        excuteDTO.setAccountGrade(accountGrade);
        // 发货数量
        BigDecimal receivedWeight = saleStmtDetails.stream().map(ErpSaleStmtDetail::getSrcNum)
                .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        excuteDTO.setReceivedWeight(receivedWeight);
        //结算数量
        BigDecimal accountWeight = saleStmtDetails.stream().map(ErpSaleStmtDetail :: getRealNum)
                .reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        excuteDTO.setAccountWeight(accountWeight);
        // 干重盈亏
        excuteDTO.setDryBalance(accountWeight.subtract(receivedWeight));
        // 品位盈亏
        excuteDTO.setGradeBalance(accountGrade.subtract(receivedGrade));
        // 已付金额
        List<ErpReceiptOrderDetail> erpReceiptOrderDetails = receiptOrderDetailMapper
                .selectList(new EntityWrapper<>(new ErpReceiptOrderDetail()
                        .setSaleOrderId(id).setDeleted(Status.FALSE.getKey())));
        excuteDTO.setPayAmount(erpReceiptOrderDetails.stream().map(ErpReceiptOrderDetail::getReceiptAmount)
                .reduce(BigDecimal::add).orElse(BigDecimal.ZERO));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,excuteDTO);
    }

    /**
     * 根据订单id获取订单基本信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpSaleOrderQueryDTO> getSaleOrderBaseInfo(Integer id) {
        List<ErpSaleOrderQueryDTO> saleOrderInfoList = erpSaleOrderDetailMapper.findSaleOrderInfo(id);
        ErpSaleOrderQueryDTO saleOrder = null;
        List<ErpProductBatchListDTO> productBatchList = null;
        ErpProductBatchListDTO productBatch = null;
        if (CollectionUtils.isNotEmpty(saleOrderInfoList)) {
            productBatchList = new ArrayList<>();
            ErpSaleOrderQueryDTO erpSaleOrderQueryDTO = saleOrderInfoList.get(0);
            saleOrder = new ErpSaleOrderQueryDTO();
            saleOrder.setProductName(erpSaleOrderQueryDTO.getProductName())
                    .setCompanyName(erpSaleOrderQueryDTO.getCompanyName())
                    .setCompany(erpSaleOrderQueryDTO.getCompany())
                    .setProductId(erpSaleOrderQueryDTO.getProductId())
                    .setCustomerName(erpSaleOrderQueryDTO.getCustomerName());
            List<Integer> batchIds = new ArrayList<>(saleOrderInfoList.size());
            for (ErpSaleOrderQueryDTO order : saleOrderInfoList) {
                if (Objects.nonNull(order.getProductBatchId()) && StringUtils.isNotBlank(order.getBatchName())) {
                    if (batchIds.contains(order.getProductBatchId())) {
                        continue;
                    }
                    batchIds.add(order.getProductBatchId());
                    productBatch = new ErpProductBatchListDTO();
                    productBatch.setProductBatchId(order.getProductBatchId())
                            .setBatchName(order.getBatchName())
                            .setProductId(order.getProductId());
                    productBatchList.add(productBatch);
                }
            }
            saleOrder.setProductBatchList(productBatchList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrder);
    }
}

package com.bee.platform.datadriver.service.impl;

import com.alibaba.fastjson.JSONArray;
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
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseOrderMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseStatementMapper;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseStmtDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpTestReportMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.entity.ErpPurchaseStatement;
import com.bee.platform.datadriver.entity.ErpPurchaseStmtDetail;
import com.bee.platform.datadriver.enums.EnumPurchaseGoodsOrderStatus;
import com.bee.platform.datadriver.rq.ErpPurchaseStatementRQ;
import com.bee.platform.datadriver.rq.PurchaseStatementRQ;
import com.bee.platform.datadriver.service.ErpPurchaseStatementService;
import com.bee.platform.datadriver.service.ErpPurchaseStmtDetailService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import io.swagger.models.auth.In;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.*;

/**
 * <p>
 * 采购结算单 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpPurchaseStatementServiceImpl extends ServiceImpl<ErpPurchaseStatementMapper, ErpPurchaseStatement> implements ErpPurchaseStatementService {

    @Autowired
    private ErpPurchaseStatementMapper purchaseStatementMapper;
    @Autowired
    private ErpPurchaseStmtDetailMapper purchaseStmtDetailMapper;
    @Autowired
    private ErpPurchaseOrderMapper purchaseOrderMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpTestReportMapper testReportMapper;
    @Autowired
    private ErpPurchaseStmtDetailService purchaseStmtDetailService;

    /**
     * @Description 新增采购结算
     * @Param purchaseStatementRQ
     * @Date 2019/5/30 16:27
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> add(AuthPlatformUserInfo simpleUserInfo, PurchaseStatementRQ purchaseStatementRQ) {
        //校验结算单号是否存在
        ErpPurchaseStatement statement = selectOne(new EntityWrapper<>(new ErpPurchaseStatement()
                .setCode(purchaseStatementRQ.getCode())
                .setCompanyId(purchaseStatementRQ.getCompanyId())
                .setDeleted(Status.FALSE.getKey())));
        if (Objects.nonNull(statement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_STATEMENT_ORDER_CODE_EXIST);
        }
        ErpPurchaseStatement purchaseStatement = new ErpPurchaseStatement()
                .setAmount(purchaseStatementRQ.getRealAmount())
                .setDryBalance(purchaseStatementRQ.getDryBalance())
                .setRealDryWeight(purchaseStatementRQ.getRealDryWeight())
                .setSrcDryWeight(purchaseStatementRQ.getSrcDryWeight())
                .setOrderId(purchaseStatementRQ.getOrderId())
                .setStatementTime(DateUtils.parse(purchaseStatementRQ.getStatementTime(), DateUtils.Y_M_D))
                .setState(EnumPurchaseGoodsOrderStatus.SAVED.getKey())
                .setUrl(purchaseStatementRQ.getUrl())
                .setCreateUser(simpleUserInfo.getId())
                .setCompanyId(purchaseStatementRQ.getCompanyId())
                .setEnterpriseId(simpleUserInfo.getOrgId())
                .setCreateTime(new Date());
        purchaseStatement.setCode(purchaseStatementRQ.getCode());
        purchaseStatementMapper.insert(purchaseStatement);

        if (!CollectionUtils.isEmpty(purchaseStatementRQ.getPurchaseStmtList())) {
            List<ErpPurchaseStmtDetail> stmtDetails = BeanUtils.assemble(ErpPurchaseStmtDetail.class, purchaseStatementRQ.getPurchaseStmtList());
            stmtDetails.forEach(stmt -> {
                stmt.setStatementId(purchaseStatement.getId())
                        .setCreateTime(new Date());
            });
            purchaseStmtDetailService.insertBatch(stmtDetails);
        }

        //更新采购订单结算状态
        purchaseOrderMapper.updateById(new ErpPurchaseOrder()
                .setId(purchaseStatementRQ.getOrderId()).
                        setAccountState(Status.TRUE.getKey()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, purchaseStatement.getId());
    }

    /**
     * @Description 编辑采购结算
     * @Param purchaseStatementRQ
     * @Date 2019/5/30 19:09
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> update(AuthPlatformUserInfo simpleUserInfo, PurchaseStatementRQ purchaseStatementRQ) {
        ErpPurchaseStatement purchaseStatement = selectOne(new EntityWrapper<>(new ErpPurchaseStatement()
                .setId(purchaseStatementRQ.getId()).setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(purchaseStatement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_STATEMENT_ORDER_NOT_EXIST);
        }
        //校验结算单号是否存在
        ErpPurchaseStatement existStatement = selectOne(new EntityWrapper<>(new ErpPurchaseStatement()
                .setCode(purchaseStatementRQ.getCode())
                .setCompanyId(purchaseStatementRQ.getCompanyId())
                .setDeleted(Status.FALSE.getKey())));
        if (Objects.nonNull(existStatement) && !existStatement.getId().equals(purchaseStatementRQ.getId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_STATEMENT_ORDER_CODE_EXIST);
        }
        ErpPurchaseStatement statement = BeanUtils.copyProperties(purchaseStatementRQ, ErpPurchaseStatement.class);
        statement.setId(purchaseStatementRQ.getId()).setUpdateUser(simpleUserInfo.getId())
                .setStatementTime((DateUtils.parse(purchaseStatementRQ.getStatementTime(), DateUtils.Y_M_D)));
        purchaseStatementMapper.updateById(statement);

        purchaseStmtDetailService.update(new ErpPurchaseStmtDetail().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpPurchaseStmtDetail().setStatementId(purchaseStatementRQ.getId())));
        if (!CollectionUtils.isEmpty(purchaseStatementRQ.getPurchaseStmtList())) {
            List<ErpPurchaseStmtDetail> stmtDetails = BeanUtils.assemble(ErpPurchaseStmtDetail.class, purchaseStatementRQ.getPurchaseStmtList());
            stmtDetails.forEach(stmt -> {
                stmt.setStatementId(purchaseStatement.getId())
                        .setCreateTime(new Date());
            });
            purchaseStmtDetailService.insertBatch(stmtDetails);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, purchaseStatementRQ.getId());
    }

    /**
     * @Description 批量删除采购结算
     * @Date 2019/5/30 19:33
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> batchDelete(String ids) {
        String[] idArray = ids.split(",");
        if (idArray.length <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PURCHASE_STATEMENT_ORDER_ID_EMPTY);
        }
        List<Integer> idList = new ArrayList<Integer>();
        for (String id : idArray) {
            idList.add(Integer.valueOf(id));
        }
        //删除采购结算单
        purchaseStatementMapper.batchDelete(idList);
        for (int i=0;i<idArray.length;i++){
            ErpPurchaseStatement statement = purchaseStatementMapper.selectById(new ErpPurchaseStatement()
                    .setId(Integer.valueOf(idArray[i])));
            List<ErpPurchaseStatement> statementList = purchaseStatementMapper.selectList(new EntityWrapper<>(new ErpPurchaseStatement()
                    .setOrderId(statement.getOrderId()).setDeleted(Status.FALSE.getKey())));
            if (CollectionUtils.isEmpty(statementList)){
                // 修改订单结算状态
                if (purchaseOrderMapper.update(new ErpPurchaseOrder()
                        .setAccountState(Status.FALSE.getKey())
                        .setUpdateTime(new Date()),new EntityWrapper<ErpPurchaseOrder>()
                        .eq("id",statement.getOrderId()).and()
                        .eq("deleted",Status.FALSE.getKey())) < 0){
                    throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERP_INVOICE_STATE_UPDATE_FAILED);
                }
            }
        }
        //删除采购结算明细
        purchaseStmtDetailMapper.batchDelete(idList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 查询采购结算单详情
     * @Param id
     * @Date 2019/5/30 19:54
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ErpPurchaseStatementDTO> findStatementInfo(Integer id) {
        ErpPurchaseStatement purchaseStatement = purchaseStatementMapper.selectOne(new ErpPurchaseStatement()
                .setId(id).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(purchaseStatement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        ErpPurchaseStatementDTO purchaseStatementDto = BeanUtils.copyProperties(purchaseStatement, ErpPurchaseStatementDTO.class);
        List<ErpPurchaseStatementDetailDTO> purchaseStatementDetails = purchaseStmtDetailMapper.getPurchaseStatementsByStatementId(id);
        if (!CollectionUtils.isEmpty(purchaseStatementDetails)) {
            ErpPurchaseStatementDetailDTO stmtDto = purchaseStatementDetails.get(0);
            Integer productBatchId = stmtDto.getProductBatchId();
            if (ObjectUtils.isEmpty(productBatchId)) {
                productBatchId = 0;
            }
            purchaseStatementDto.setBatchName(stmtDto.getBatchName()).setProductBatchId(productBatchId);
            purchaseStatementDetails.forEach(stmt -> {
                stmt.setCode(purchaseStatement.getCode());
            });
            purchaseStatementDto.setPurchaseStmtList(purchaseStatementDetails);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, purchaseStatementDto);
    }

    /**
     * @Description 更新采购结算单状态
     * @Param id
     * @Param state
     * @Date 2019/5/30 20:31
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateState(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state) {
        purchaseStatementMapper.updateById(new ErpPurchaseStatement()
                .setId(id).setState(state).setUpdateUser(simpleUserInfo.getId()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    /**
     * @Description 条件查询采购结算单
     * @Param erpPurchaseStatementRQ
     * @Param pagination
     * @Date 2019/5/30 20:50
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<ErpPurchaseStatementDetailDTO> findStatementList(Integer companyId,
                                                                 ErpPurchaseStatementRQ erpPurchaseStatementRQ,
                                                                 Pagination pagination) {
        if (Objects.isNull(erpPurchaseStatementRQ.getCompanyId())) {
            List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
            if (!CollectionUtils.isEmpty(enterprises)) {
                List<Integer> enterpriseIds = new ArrayList<Integer>();
                enterprises.forEach(enterprise -> {
                    enterpriseIds.add(enterprise.getValue());
                });
                erpPurchaseStatementRQ.setEnterpriseIdList(enterpriseIds);
            }
        }
        List<ErpPurchaseStatementDetailDTO> statementList = purchaseStatementMapper.findStatementList(erpPurchaseStatementRQ, pagination);
        if (!CollectionUtils.isEmpty(statementList)) {
            statementList.forEach(stmt -> {
                if (StringUtils.isNotBlank(stmt.getProductName()) && StringUtils.isNotBlank(stmt.getBatchName())) {
                    stmt.setProductName(stmt.getProductName() + "-" + stmt.getBatchName());
                }
            });
        }
        return statementList;
    }

    /**
     * @Description 根据采购合同id查询验收情况
     * @Param id
     * @Date 2019/6/6 9:57
     * @Author xin.huang
     * @Return
     */
//    @Override
//    public ResponseResult findTestReportInfo(Integer id) {
//        List<ErpPurchaseStatementReportDTO> testReportList = testReportMapper.findPurchaseStatementInfoByOrderID(id);
//        if (!CollectionUtils.isEmpty(testReportList)) {
//            testReportList.forEach(report -> {
//                if (StringUtils.isNotBlank(report.getResult())) {
//                    List<ErpTestReportDetailDTO> reportList = (List<ErpTestReportDetailDTO>)JSONArray.parseArray(report.getResult(), ErpTestReportDetailDTO.class);
//                    report.setReportList(reportList);
//                }
//            });
//        }
//        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, testReportList);
//    }
    @Override
    public ResponseResult findTestReportInfo(Integer orderId, Integer productBatchId) {
        List<ErpPurchaseStatementReportDTO> testReportList = testReportMapper
                .findPurchaseStatementInfoByOrderId(orderId, productBatchId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, cacluateResult(testReportList));
    }

    /**
     * 计算验收情况各试验结果
     *
     * @param testReportList
     * @return
     */
    private static ErpPurchaseStatementReportReturnDTO cacluateResult(List<ErpPurchaseStatementReportDTO> testReportList) {
        ErpPurchaseStatementReportReturnDTO result = new ErpPurchaseStatementReportReturnDTO();
        if (CollectionUtils.isEmpty(testReportList)) {
            return result;
        }
        //拼装表头
        List<Map<String, String>> titleList = new ArrayList<>();
        Map<String, String> title = Maps.newLinkedHashMap();
        title.put("recordTime", "到厂日期");
        String numTitle = "到厂数量";
        //产品单位
        String unit = testReportList.get(0).getUnit();
        if (StringUtils.isNotBlank(unit)) {
            numTitle = numTitle + "(" + unit + ")";
        }
        title.put("num", numTitle);
        title.put("code", "化验单号");
        for (ErpPurchaseStatementReportDTO report : testReportList) {
            if (StringUtils.isBlank(report.getResult())) {
                continue;
            }
            List<ErpTestReportDetailDTO> reportList = JSONArray.parseArray(report.getResult(), ErpTestReportDetailDTO.class);
            report.setReportList(reportList);
            report.getReportList().forEach(r -> {
                if (!title.containsKey(r.getTestItem())) {
                    title.put(r.getTestItem(), r.getTestItem());
                }
            });
        }

        for (Map.Entry<String, String> entry : title.entrySet()) {
            Map<String, String> titleItem = new HashMap<>();
            titleItem.put("name", entry.getValue());
            titleItem.put("key", entry.getKey());
            titleList.add(titleItem);
        }
        result.setTitle(titleList);

        // 合计的map
        Map<String, BigDecimal> totalMap = Maps.newHashMap();
        // item检测属性返回的List
        List<Map<String, String>> reportItemList = Lists.newArrayList();
        Map<String, BigDecimal> numMap = Maps.newHashMap();
        for (ErpPurchaseStatementReportDTO report : testReportList) {
            // 统计到厂数量
            if (totalMap.get("num") == null) {
                totalMap.put("num", report.getNum());
            } else {
                totalMap.put("num", totalMap.get("num").add(report.getNum()));
            }
            // 记录每个检测属性的值
            Map<String, String> itemMap = Maps.newHashMap();
            itemMap.put("recordTime", DateUtils.format(report.getRecordTime(), DateUtils.Y_M_D));
            itemMap.put("num", report.getNum().toString());
            itemMap.put("code", report.getCode() == null ? "" : report.getCode());
            if (!CollectionUtils.isEmpty(report.getReportList())) {
                report.getReportList().forEach(r -> {
                    if (numMap.get(r.getTestItem() + "-num") == null) {
                        numMap.put(r.getTestItem() + "-num", report.getNum());
                    } else {
                        numMap.put(r.getTestItem() + "-num", numMap.get(r.getTestItem() + "-num").add(report.getNum()));
                    }
                    String testValue = "0";
                    if (StringUtils.isNotBlank(r.getTestValue())) {
                        testValue = r.getTestValue();
                    }
                    itemMap.put(r.getTestItem(), testValue);
                    // 统计其他检测属性
                    BigDecimal value;
                    try {
                        value = new BigDecimal(testValue);
                    } catch (NumberFormatException e) {
                        value = BigDecimal.ZERO;
                    }
                    if (totalMap.get(r.getTestItem()) == null) {
                        totalMap.put(r.getTestItem(), report.getNum().multiply(value));
                    } else {
                        totalMap.put(r.getTestItem(), totalMap.get(r.getTestItem())
                                .add(report.getNum().multiply(value)));
                    }
                });
            }
            reportItemList.add(itemMap);
        }
        result.setReportItemList(reportItemList);

        // 拼装合计数量
        BigDecimal totalNum = totalMap.get("num");
        if (totalNum.compareTo(BigDecimal.ZERO) == 1) {
            for (Map.Entry<String, BigDecimal> entry : totalMap.entrySet()) {
                if (!"num".equals(entry.getKey())) {
                    BigDecimal numValue = numMap.get(entry.getKey() + "-num");
                    if (Objects.isNull(numValue)) {
                        continue;
                    }
                    BigDecimal cacluateNum = BigDecimal.ZERO;
                    if (numValue.compareTo(BigDecimal.ZERO) == 1) {
                        cacluateNum = entry.getValue().divide(numValue, 2, BigDecimal.ROUND_HALF_UP);
                    }
                    totalMap.put(entry.getKey(), cacluateNum);
                }
            }
        }
        result.setTotal(totalMap);

        return result;
    }

}

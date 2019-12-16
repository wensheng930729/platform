package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.ErpSaleStatementMapper;
import com.bee.platform.datadriver.dao.mapper.ErpSaleStmtDetailMapper;
import com.bee.platform.datadriver.dto.ErpSaleStatementDTO;
import com.bee.platform.datadriver.dto.ErpSaleStatementDetailDTO;
import com.bee.platform.datadriver.entity.ErpSaleOrder;
import com.bee.platform.datadriver.entity.ErpSaleStatement;
import com.bee.platform.datadriver.entity.ErpSaleStmtDetail;
import com.bee.platform.datadriver.enums.EnumErpSaleOrderStatus;
import com.bee.platform.datadriver.enums.EnumPurchaseGoodsOrderStatus;
import com.bee.platform.datadriver.rq.ErpSaleStatementDetailRQ;
import com.bee.platform.datadriver.rq.ErpSaleStatementSelectRQ;
import com.bee.platform.datadriver.rq.SaleStatementRQ;
import com.bee.platform.datadriver.service.ErpSaleOrderService;
import com.bee.platform.datadriver.service.ErpSaleStatementService;
import com.bee.platform.datadriver.service.ErpSaleStmtDetailService;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 销售结算单 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Service
public class ErpSaleStatementServiceImpl extends ServiceImpl<ErpSaleStatementMapper, ErpSaleStatement> implements ErpSaleStatementService {

    @Autowired
    private ErpSaleStatementMapper saleStatementMapper;
    @Autowired
    private ErpSaleStmtDetailService saleStmtDetailService;
    @Autowired
    private ErpSaleStmtDetailMapper saleStmtDetailMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpSaleOrderService saleOrderService;

    /**
     * @Description 新增销售结算
     * @Param saleStatementRQ
     * @Date 2019/5/31 16:02
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> add(AuthPlatformUserInfo simpleUserInfo, SaleStatementRQ saleStatementRQ) {
        ErpSaleStatement erpSaleStatement = BeanUtils.copyProperties(saleStatementRQ, ErpSaleStatement.class);
        erpSaleStatement.setState(EnumPurchaseGoodsOrderStatus.SAVED.getKey());
        //校验结算单号是否存在
        ErpSaleStatement saleStatement = selectOne(new EntityWrapper<>(new ErpSaleStatement()
                .setCode(saleStatementRQ.getCode())
                .setCompanyId(saleStatementRQ.getCompanyId())
                .setDeleted(Status.FALSE.getKey())));
        if (Objects.nonNull(saleStatement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_STATEMENT_ORDER_CODE_EXIST);
        }
        erpSaleStatement.setCreateUser(simpleUserInfo.getId()).setEnterpriseId(simpleUserInfo.getOrgId()).setCreateTime(new Date());
        saleStatementMapper.insert(erpSaleStatement);
        List<ErpSaleStatementDetailRQ> saleStatementDetailList = saleStatementRQ.getSaleStatementDetailList();
        if (!CollectionUtils.isEmpty(saleStatementDetailList)) {
            List<ErpSaleStmtDetail> saleStmtDetails = BeanUtils.assemble(ErpSaleStmtDetail.class, saleStatementDetailList);
            saleStmtDetails.forEach(ssd -> {
                ssd.setStatementId(erpSaleStatement.getId());
                ssd.setCreateTime(new Date());
            });
            saleStmtDetailService.insertBatch(saleStmtDetails);
        }
        //更新销售订单结算状态
        updateSaleOrderState(saleStatementRQ.getSaleOrderId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpSaleStatement.getId());
    }

    /**
     * @Description 更新销售结算
     * @Param id
     * @Param saleStatementRQ
     * @Date 2019/5/31 16:54
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> update(AuthPlatformUserInfo simpleUserInfo, SaleStatementRQ saleStatementRQ) {
        ErpSaleStatement erpSaleStatement = selectOne(new EntityWrapper<>(new ErpSaleStatement()
                .setId(saleStatementRQ.getId()).setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(erpSaleStatement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_STATEMENT_ORDER_NOT_EXIST);
        }
        //校验结算单号是否存在
        ErpSaleStatement existSaleStatement = selectOne(new EntityWrapper<>(new ErpSaleStatement()
                .setCode(saleStatementRQ.getCode())
                .setCompanyId(saleStatementRQ.getCompanyId())
                .setDeleted(Status.FALSE.getKey())));
        if (Objects.nonNull(existSaleStatement) && !existSaleStatement.getId().equals(saleStatementRQ.getId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_STATEMENT_ORDER_CODE_EXIST);
        }
        ErpSaleStatement saleStatement = BeanUtils.copyProperties(saleStatementRQ, ErpSaleStatement.class);
        saleStatement.setUpdateUser(simpleUserInfo.getId());
        saleStatementMapper.updateById(saleStatement);
        saleStmtDetailService.update(new ErpSaleStmtDetail().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<ErpSaleStmtDetail>().eq("statement_id", saleStatementRQ.getId()));
        List<ErpSaleStatementDetailRQ> saleStatementDetailList = saleStatementRQ.getSaleStatementDetailList();
        if (!CollectionUtils.isEmpty(saleStatementDetailList)) {
            List<ErpSaleStmtDetail> saleStmtDetails = BeanUtils
                    .assemble(ErpSaleStmtDetail.class, saleStatementDetailList);
            saleStmtDetails.forEach(ssd -> {
                ssd.setStatementId(saleStatementRQ.getId());
            });
            saleStmtDetailService.insertBatch(saleStmtDetails);
        }
        //更新销售订单结算状态
        updateSaleOrderState(saleStatementRQ.getSaleOrderId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleStatementRQ.getId());
    }

    /**
     * @Description 删除销售结算
     * @Param id
     * @Date 2019/5/29 15:43
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteSaleStatement(Integer id) {
        ErpSaleStatement saleStatement = selectOne(new EntityWrapper<>(new ErpSaleStatement()
                .setId(id).setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(saleStatement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        updateById(new ErpSaleStatement().setId(id).setDeleted(Status.TRUE.getKey()));
        //删除明细表数据
        saleStmtDetailMapper.update(new ErpSaleStmtDetail().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new ErpSaleStmtDetail().setStatementId(id)));
        //更新销售订单结算状态
        updateSaleOrderState(saleStatement.getSaleOrderId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    /**
     * @Description 查看销售结算单详情
     * @Param id
     * @Date 2019/5/29 16:35
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ErpSaleStatementDTO> findStatementOrder(Integer id) {
        ErpSaleStatement saleStatement = selectOne(new EntityWrapper<>(new ErpSaleStatement()
                .setId(id).setDeleted(Status.FALSE.getKey())));
        if (Objects.isNull(saleStatement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_STATEMENT_ORDER_NOT_EXIST);
        }
        ErpSaleStatementDTO saleStatementDTO = BeanUtils.copyProperties(saleStatement, ErpSaleStatementDTO.class);
        List<ErpSaleStmtDetail> saleStmtDetails = saleStmtDetailMapper.selectList(new EntityWrapper<ErpSaleStmtDetail>()
                .eq("statement_id", id).and()
                .eq("deleted", Status.FALSE.getKey()));

        List<ErpSaleStatementDetailDTO> saleStatementDetailList = BeanUtils.assemble(ErpSaleStatementDetailDTO.class, saleStmtDetails);
        if (!CollectionUtils.isEmpty(saleStatementDetailList)) {
            saleStatementDetailList.forEach( saleStatementDetail -> {
                saleStatementDetail.setCode(saleStatement.getCode());
            });
            saleStatementDTO.setSaleStatementDetailList(saleStatementDetailList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleStatementDTO);
    }

    /**
     * @Description 条件查询销售结算单列表
     * @Param saleStatementSelectRQ
     * @Param pagination
     * @Date 2019/5/30 14:45
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<ErpSaleStatementDetailDTO> findSaleStatementOrderList(Integer companyId,
                                                                      ErpSaleStatementSelectRQ saleStatementSelectRQ,
                                                                      Pagination pagination) {
        if (Objects.isNull(saleStatementSelectRQ.getCompanyId())) {
            List<AuthEnterpriseFlatDTO> enterprises = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
            if (!CollectionUtils.isEmpty(enterprises)) {
                List<Integer> enterpriseIds = new ArrayList<Integer>();
                enterprises.forEach(enterprise -> {
                    enterpriseIds.add(enterprise.getValue());
                });
                saleStatementSelectRQ.setEnterpriseIdList(enterpriseIds);
            }
        }
        List<ErpSaleStatementDetailDTO> statementDetailList = saleStmtDetailMapper
                .findSaleStatementOrderList(saleStatementSelectRQ, pagination);

        //计算明细表中的加权平均品位
        statisticsGrade(statementDetailList);
        return statementDetailList;
    }

    /**
     * @Description 更新销售结算单状态
     * @Param id
     * @Param state
     * @Date 2019/6/1 11:37
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateState(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state) {
        ErpSaleStatement saleStatement = saleStatementMapper.selectOne(new ErpSaleStatement()
                .setId(id).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(saleStatement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SALE_STATEMENT_ORDER_NOT_EXIST);
        }
        saleStatementMapper.update(new ErpSaleStatement().setState(state),
                new EntityWrapper<ErpSaleStatement>().eq("id", id));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    /**
     * @Description 根据销售订单id查看销售结算情况
     * @Param id
     * @Date 2019/6/12 18:56
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<ErpSaleStatementDetailDTO>> getSaleStatementInfo(Integer id) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleStmtDetailMapper.getSaleStatementInfo(id));
    }

    /**
     * @Description 更新销售订单结算状态
     * @Param orderId
     * @Date 2019/6/14 14:47
     * @Author xin.huang
     * @Return
     */
    private void updateSaleOrderState(Integer orderId) {
        //更新结算状态
        List<ErpSaleStatement> erpSaleStatements = saleStatementMapper
                .selectList(new EntityWrapper<>(new ErpSaleStatement()
                        .setSaleOrderId(orderId)
                        .setDeleted(Status.FALSE.getKey())));
        int accountState = EnumErpSaleOrderStatus.AccountType.N0T_ACCOUNTT.getKey();
        //已结算
        if (!CollectionUtils.isEmpty(erpSaleStatements)) {
            accountState = EnumErpSaleOrderStatus.AccountType.HAD_ACCOUNTT.getKey();
        }
        saleOrderService.updateById(new ErpSaleOrder().setId(orderId).setAccountState(accountState));
    }

    /**
     * @Description 计算加权品位
     * @Param statementDetailList
     */
    private void statisticsGrade(List<ErpSaleStatementDetailDTO> statementDetailList) {
        if (CollectionUtils.isEmpty(statementDetailList)) {
            return;
        }
        List<Integer> statementIds = new ArrayList<>();
        statementDetailList.forEach(statement -> {
            statementIds.add(statement.getStatementId());
        });
        List<ErpSaleStmtDetail> erpSaleStmtDetails = saleStmtDetailMapper
                .selectList(new EntityWrapper<ErpSaleStmtDetail>()
                        .in("statement_id", statementIds)
                        .and()
                        .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(erpSaleStmtDetails)) {
            return;
        }
        Map<Integer, List<ErpSaleStmtDetail>> statementMap = erpSaleStmtDetails.stream()
                .collect(Collectors.groupingBy(ErpSaleStmtDetail::getStatementId));
        Map<Integer, ErpSaleStatementDetailDTO> gradeMap = new HashMap<Integer, ErpSaleStatementDetailDTO>();
        ErpSaleStatementDetailDTO saleStatementDetailDTO;
        for (Map.Entry<Integer, List<ErpSaleStmtDetail>> entry : statementMap.entrySet()) {
            //发货品位
            BigDecimal srcGrade = BigDecimal.ZERO;
            //收货品位
            BigDecimal receiveGrade = BigDecimal.ZERO;
            //结算品位
            BigDecimal realGrade = BigDecimal.ZERO;
            //发货数量
            BigDecimal srcNum = BigDecimal.ZERO;
            //收货数量
            BigDecimal receiveNum = BigDecimal.ZERO;
            //结算数量
            BigDecimal realNum = BigDecimal.ZERO;
            for (ErpSaleStmtDetail stmt : entry.getValue()) {
                if (Objects.nonNull(stmt.getSrcGrade()) && Objects.nonNull(stmt.getSrcNum())) {
                    srcGrade = srcGrade.add(stmt.getSrcGrade().multiply(stmt.getSrcNum()));
                    srcNum = srcNum.add(stmt.getSrcNum());
                }
                if (Objects.nonNull(stmt.getReceiveGrade()) && Objects.nonNull(stmt.getReceiveNum())) {
                    receiveGrade = receiveGrade.add(stmt.getReceiveGrade().multiply(stmt.getReceiveNum()));
                    receiveNum = receiveNum.add(stmt.getReceiveNum());
                }
                if (Objects.nonNull(stmt.getRealGrade()) && Objects.nonNull(stmt.getRealNum())) {
                    realGrade = realGrade.add(stmt.getRealGrade().multiply(stmt.getRealNum()));
                    realNum = realNum.add(stmt.getRealNum());
                }
            }
            saleStatementDetailDTO = new ErpSaleStatementDetailDTO();
            //加权平均品位
            if (srcNum.compareTo(BigDecimal.ZERO) == 1) {
                srcGrade = srcGrade.divide(srcNum, 2, BigDecimal.ROUND_HALF_UP);
            }
            if (receiveNum.compareTo(BigDecimal.ZERO) == 1) {
                receiveGrade = receiveGrade.divide(receiveNum, 2, BigDecimal.ROUND_HALF_UP);
            }
            if (realNum.compareTo(BigDecimal.ZERO) == 1) {
                realGrade = realGrade.divide(realNum, 2, BigDecimal.ROUND_HALF_UP);
            }
            saleStatementDetailDTO.setSrcGrade(srcGrade);
            saleStatementDetailDTO.setReceiveGrade(receiveGrade);
            saleStatementDetailDTO.setRealGrade(realGrade);
            gradeMap.put(entry.getKey(), saleStatementDetailDTO);
        }

        statementDetailList.forEach( stmt -> {
            ErpSaleStatementDetailDTO statementDetail = gradeMap.get(stmt.getStatementId());
            if (Objects.nonNull(statementDetail)) {
                stmt.setSrcGrade(statementDetail.getSrcGrade());
                stmt.setReceiveGrade(statementDetail.getReceiveGrade());
                stmt.setRealGrade(statementDetail.getRealGrade());
            }
        });
    }
}

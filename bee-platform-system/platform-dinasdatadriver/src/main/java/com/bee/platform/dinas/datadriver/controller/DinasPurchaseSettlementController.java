package com.bee.platform.dinas.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.dinas.datadriver.dto.DinasPurchaseSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.SettlementBatchRQ;
import com.bee.platform.dinas.datadriver.rq.CancelBatchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseSettlementSearchRQ;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseSettlementService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.bee.platform.dinas.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 采购结算表 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "dinasPurchaseSettlement", tags = "砂石采购结算表相关的接口")
@RequestMapping("/dinasPurchaseSettlement")
public class DinasPurchaseSettlementController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private DinasPurchaseSettlementService purchaseSettlementService;

    @ApiOperation(value = "条件搜索采购结算列表")
    @PostMapping("/searchPurchaseSettlementByCondition")
    public ResponseResult<List<DinasPurchaseSettlementSearchDTO>> searchPurchaseSettlementByCondition(HttpServletRequest request, @RequestBody DinasPurchaseSettlementSearchRQ rq, Page page) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();

        if (ObjectUtils.isEmpty(companyId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return purchaseSettlementService.searchPurchaseSettlementByCondition(rq, page, companyId);
    }

    @Log(businessType = EnumBusinessType.PURCHASE_SETTLEMENT, operateType = OperateType.CANCEL_SETTLEMENT, isBatch = true)
    @ApiOperation(value = "根据ids撤销采购结算")
    @PostMapping("/cancelSettlementByIds")
    public ResponseResult<List<Integer>> cancelSettlementByIds(HttpServletRequest request, @RequestBody @Valid CancelBatchRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        List<Integer> ids = purchaseSettlementService.cancelSettlementByIds(userInfo, rq.getIds());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, ids);
    }

    @Log(businessType = EnumBusinessType.PURCHASE_SETTLEMENT, operateType = OperateType.SETTLEMENT)
    @ApiOperation(value = "采购结算一条记录")
    @PostMapping("/settlementOne")
    public ResponseResult<Integer> settlementOne(HttpServletRequest request, @RequestBody @Valid SettlementRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer id = purchaseSettlementService.settlementOne(userInfo, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Log(businessType = EnumBusinessType.PURCHASE_SETTLEMENT, operateType = OperateType.SETTLEMENT, isBatch = true)
    @ApiOperation(value = "采购结算一批记录")
    @PostMapping("/settlementBatch")
    public ResponseResult<List<Integer>> settlementBatch(HttpServletRequest request, @RequestBody @Valid SettlementBatchRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<Integer> ids = purchaseSettlementService.settlementBatch(userInfo, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, ids);
    }

    @ApiOperation(value = "根据合同id查询可以余额")
    @GetMapping("/getAvailableAmount")
    public ResponseResult<BigDecimal> getAvailableAmount(Integer contractId) {

        BigDecimal availableAmount = purchaseSettlementService.getAvailableAmount(contractId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, availableAmount);
    }


}


package com.bee.platform.dinas.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.dinas.datadriver.dto.DinasSaleSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.*;
import com.bee.platform.dinas.datadriver.service.DinasSaleSettlementService;
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
 * 销售结算表 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "dinasSaleSettlement", tags = "砂石销售结算表相关的接口")
@RequestMapping("/dinasSaleSettlement")
public class DinasSaleSettlementController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private DinasSaleSettlementService saleSettlementService;

    @ApiOperation(value = "条件搜索销售结算列表")
    @PostMapping("/searchSaleSettlementByCondition")
    public ResponseResult<List<DinasSaleSettlementSearchDTO>> searchSaleSettlementByCondition(HttpServletRequest request, @RequestBody DinasSaleSettlementSearchRQ rq, Page page) {
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

        return saleSettlementService.searchSaleSettlementByCondition(rq, page, companyId);
    }

    @Log(businessType = EnumBusinessType.SALE_SETTLEMENT, operateType = OperateType.CANCEL_SETTLEMENT, isBatch = true)
    @ApiOperation(value = "根据ids撤销销售结算")
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
        List<Integer> ids = saleSettlementService.cancelSettlementByIds(userInfo, rq.getIds());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, ids);
    }

    @Log(businessType = EnumBusinessType.SALE_SETTLEMENT, operateType = OperateType.SETTLEMENT)
    @ApiOperation(value = "销售结算一条记录")
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
        Integer id = saleSettlementService.settlementOne(userInfo, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Log(businessType = EnumBusinessType.SALE_SETTLEMENT, operateType = OperateType.SETTLEMENT, isBatch = true)
    @ApiOperation(value = "销售结算一批记录")
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
        List<Integer> ids = saleSettlementService.settlementBatch(userInfo, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, ids);
    }

    @ApiOperation(value = "根据合同id查询可以余额")
    @GetMapping("/getAvailableAmount")
    public ResponseResult<BigDecimal> getAvailableAmount(Integer contractId) {

        BigDecimal availableAmount = saleSettlementService.getAvailableAmount(contractId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, availableAmount);
    }


}


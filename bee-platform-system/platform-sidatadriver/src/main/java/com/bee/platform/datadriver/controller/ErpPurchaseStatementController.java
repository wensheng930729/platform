package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpPurchaseStatementDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseStatementDetailDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpPurchaseStatementRQ;
import com.bee.platform.datadriver.rq.PurchaseStatementRQ;
import com.bee.platform.datadriver.service.ErpPurchaseStatementService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 采购结算单 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/erpPurchaseStatement")
@Api(value = "采购结算操作相关接口", tags = "采购结算操作相关接口")
public class ErpPurchaseStatementController {

    @Autowired
    private ErpPurchaseStatementService purchaseStatementService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @ApiOperation(value = "新增采购结算单", notes = "新增采购结算单")
    @Log(businessType= EnumBusinessType.PURCHASE_STATEMENT, operateType = OperateType.ADD)
    @PostMapping("/add")
    public ResponseResult<Integer> addPurchaseStatement(HttpServletRequest request, @RequestBody() @Valid PurchaseStatementRQ purchaseStatementRQ) {
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return purchaseStatementService.add(simpleUserInfo, purchaseStatementRQ);
    }

    @ApiOperation(value = "编辑采购结算单", notes = "编辑采购结算单")
    @Log(businessType= EnumBusinessType.PURCHASE_STATEMENT, operateType = OperateType.EDIT)
    @PostMapping("/update")
    public ResponseResult<Integer> updatePurchaseStatement(HttpServletRequest request, @RequestBody() @Valid PurchaseStatementRQ purchaseStatementRQ) {
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return purchaseStatementService.update(simpleUserInfo, purchaseStatementRQ);
    }

    @ApiOperation(value = "批量删除采购结算单", notes = "批量删除采购结算单")
    @ApiImplicitParam(name = "ids", value = "销售结算单id,多个id以逗号分隔", required = true)
    @PostMapping("/delete")
    public ResponseResult<ResCodeEnum> deletePurchaseStatement(String ids) {
        return purchaseStatementService.batchDelete(ids);
    }

    @ApiOperation(value = "查询采购结算单详情", notes = "查询采购结算单详情")
    @ApiImplicitParam(name = "id", value = "采购结算单id", required = true)
    @GetMapping("/info")
    public ResponseResult<ErpPurchaseStatementDTO> findStatementInfo(Integer id) {
        return purchaseStatementService.findStatementInfo(id);
    }

    @ApiOperation(value = "条件查询采购结算单", notes = "条件查询采购结算单")
    @PostMapping("/list")
    public ResponseResult<List<ErpPurchaseStatementDetailDTO>> findStatementList(HttpServletRequest request, @RequestBody()ErpPurchaseStatementRQ erpPurchaseStatementRQ, Page page) {
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (ObjectUtils.isEmpty(companyId) && Objects.isNull(erpPurchaseStatementRQ.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ErpPurchaseStatementDetailDTO> purchaseStatementList = purchaseStatementService.findStatementList(companyId, erpPurchaseStatementRQ, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, purchaseStatementList, PageUtils.transToPage(pagination));
    }

    @ApiOperation(value = "更新采购结算单状态", notes = "更新采购结算单状态")
    @ApiImplicitParams({@ApiImplicitParam(name = "id", value = "采购结算单id", required = true),
            @ApiImplicitParam(name = "state", value = "状态 0未通过 1通过", required = true)})
    @Log(businessType= EnumBusinessType.PURCHASE_STATEMENT, operateType = OperateType.EDIT)
    @PostMapping("/updateState")
    public ResponseResult<Integer> updateState(HttpServletRequest request, Integer id, Integer state) {
        AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(simpleUserInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return purchaseStatementService.updateState(simpleUserInfo, id, state);
    }

    @ApiOperation(value = "根据采购合同id和产品批次id查询验收情况", notes = "根据采购合同id和产品批次id查询验收情况")
    @GetMapping("/getTestReportInfo")
    public ResponseResult findTestReportInfo(@RequestParam("orderId") Integer orderId,
                                             @RequestParam(value = "batchId", required = false) Integer batchId) {
        return purchaseStatementService.findTestReportInfo(orderId, batchId);
    }
}


package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpInvoiceOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrderDetail;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.rq.ErpInvoiceOrderDetailRQ;
import com.bee.platform.datadriver.rq.ErpPurchaseInvoiceOrderSaveRQ;
import com.bee.platform.datadriver.rq.ErpSaleInvoiceOrderSaveRQ;
import com.bee.platform.datadriver.service.ErpPurchaseInvoiceOrderDetailService;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import org.springframework.stereotype.Controller;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 采购发票明细 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpPurchaseInvoiceOrderDetail", tags = "erp采购发票明细相关接口")
@RequestMapping("/erpPurchaseInvoiceOrderDetail")
public class ErpPurchaseInvoiceOrderDetailController {

    @Autowired
    private ErpPurchaseInvoiceOrderDetailService purchaseInvoiceOrderDetailService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @GetMapping("/listPurchaseInvoiceOrderDetail")
    @ApiOperation(value = "获取采购发票明细列表")
    public ResponseResult<List<ErpInvoiceOrderDetailDTO>> listPurchaseInvoiceOrderDetail(String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseInvoiceOrderDetailService.listPurchaseInvoiceOrderDetail(id);
    }

    @GetMapping("/deletePurchaseInvoiceOrderDetail")
    @ApiOperation(value = "删除采购发票明细信息")
    public ResponseResult<ResCodeEnum> deletePurchaseInvoiceOrderDetail(HttpServletRequest request, String id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseInvoiceOrderDetailService.deletePurchaseInvoiceOrderDetail(userInfo,id);
    }

    @PostMapping("/savePurchaseInvoiceOrderDetail")
    @ApiOperation(value = "保存采购发票明细发票")
    public ResponseResult<ResCodeEnum> savePurchaseInvoiceOrderDetail(HttpServletRequest request, @RequestBody ErpInvoiceOrderDetailRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseInvoiceOrderDetailService.savePurchaseInvoiceOrderDetail(userInfo,rq);
    }
}


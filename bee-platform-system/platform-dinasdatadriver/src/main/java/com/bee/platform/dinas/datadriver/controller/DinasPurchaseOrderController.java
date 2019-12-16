package com.bee.platform.dinas.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrderDetail;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.DinasOrderQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseAdjustRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseOrderAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseOrderUpdateRQ;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseAdjustService;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseOrderService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.bee.platform.dinas.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 采购订单 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */

@RestController
@Api(value = "DinasPurchaseOrderController",tags = "砂石采购订单相关接口")
@Slf4j
@RequestMapping("/dinasPurchaseOrder")
public class DinasPurchaseOrderController {

    @Autowired
    private DinasPurchaseOrderService purchaseOrderService;
    @Autowired
    private DinasPurchaseAdjustService purchaseAdjustService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping("/list")
    @ApiOperation(value = "分页查询采购订单列表")
    public ResponseResult<List<DinasPurchaseOrderListDTO>> listPurchaseOrder(HttpServletRequest request, @RequestBody DinasOrderQueryRQ rq , Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<DinasPurchaseOrderListDTO> list = purchaseOrderService.listPurchaseOrder(pagination,rq,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/get")
    @ApiOperation(value = "查询采购订单详情")
    public ResponseResult<DinasPurchaseOrderInfoDTO> getPurchaseOrderDetail(@RequestParam Integer id){
        if (id == null) {
            log.info("订单id为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,purchaseOrderService.getPurchaseOrderDetail(id));
    }

    @GetMapping("/delete")
    @Log(businessType= EnumBusinessType.PURCHASE, operateType = OperateType.DELETE,isBatch = true)
    @ApiOperation(value = "删除采购订单")
    public ResponseResult<List<Integer>> deletePurchaseOrder(HttpServletRequest request, @RequestParam List<Integer> ids){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (CollectionUtils.isEmpty(ids)) {
            log.info("订单id为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.deletePurchaseOrder(userInfo,ids);
    }

    @PostMapping("/add")
    @Log(businessType= EnumBusinessType.PURCHASE, operateType = OperateType.ADD)
    @ApiOperation(value = "添加采购订单")
    public ResponseResult<Integer> addPurchaseOrder(HttpServletRequest request, @RequestBody @Valid DinasPurchaseOrderAddRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("订单信息为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.addPurchaseOrder(userInfo,rq);
    }

    @PostMapping("/update")
    @ApiOperation(value = "修改采购订单")
    @Log(businessType= EnumBusinessType.PURCHASE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updatePurchaseOrder(HttpServletRequest request, @RequestBody @Valid DinasPurchaseOrderUpdateRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("订单信息为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.updatePurchaseOrder(userInfo,rq);
    }

    @PostMapping("/addAdjust")
    @ApiOperation(value = "添加采购调价")
    public ResponseResult<Integer> addPurchaseAdjust(HttpServletRequest request, @RequestBody @Valid DinasPurchaseAdjustRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("订单信息为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseAdjustService.addPurchaseAdjust(userInfo,rq);
    }

    @GetMapping("/getOrderProduct")
    @ApiOperation(value = "获取采购订单使用的产品")
    public ResponseResult<List<DinasProductListDTO>> getOrderProduct(@RequestParam Integer id){
        if (id == null) {
            log.info("订单id为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.getOrderProduct(id);
    }

    @GetMapping("/getOrderProductSpec")
    @ApiOperation(value = "根据产品id获取采购订单使用的规格")
    public ResponseResult<List<DinasProductSpecListDTO>> getOrderProductSpec(@RequestParam Integer orderId, @RequestParam Integer productId){
        if (orderId == null || productId == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.getOrderProductSpec(orderId,productId);
    }

    @GetMapping("/getPriceByProductSpec")
    @ApiOperation(value = "根据产品id、规格查询调价前价格")
    public ResponseResult<DinasPurchaseOrderDetail> getPriceByProductSpec(@RequestParam Integer orderId, @RequestParam Integer productId, @RequestParam Integer specId){
        if (orderId == null || productId == null || specId == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.getPriceByProductSpec(orderId,productId,specId);
    }

    @GetMapping("/getOrderPullList")
    @ApiOperation(value = "查询采购订单下拉列表")
    public ResponseResult<List<DinasPurchaseOrderPullListDTO>> getOrderPullList(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,purchaseOrderService.getOrderPullList(userInfo));
    }
}


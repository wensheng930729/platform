package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.OrderDeleteRQ;
import com.bee.platform.datadriver.rq.OrderQueryRQ;
import com.bee.platform.datadriver.rq.PurchaseOrderSaveRQ;
import com.bee.platform.datadriver.service.ErpPurchaseOrderService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 采购订单 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpPurchaseOrder", tags = "erp采购订单相关接口")
@RequestMapping("/erpPurchaseOrder")
public class ErpPurchaseOrderController{

    @Autowired
    private ErpPurchaseOrderService purchaseOrderService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/listErpPurchaseOrder")
    @ApiOperation(value = "分页查询采购订单列表")
    public ResponseResult<List<ErpPurchaseOrderInfoDTO>> listErpPurchaseOrder(HttpServletRequest request, @RequestBody OrderQueryRQ rq , Page page){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpPurchaseOrderInfoDTO> list = purchaseOrderService.listErpPurchaseOrder(pagination,rq,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/getErpPurchaseOrder")
    @ApiOperation(value = "获取采购订单信息")
    public ResponseResult<ErpPurchaseOrderDTO> getErpPurchaseOrder(HttpServletRequest request, Integer id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (id == null) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.getErpPurchaseOrder(userInfo,id);
    }

    @PostMapping("/saveErpPurchaseOrder")
    @ApiOperation(value = "新增采购订单")
    public ResponseResult<Integer> saveErpPurchaseOrder(HttpServletRequest request, @RequestBody PurchaseOrderSaveRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.saveErpPurchaseOrder(userInfo,rq);
    }

    @GetMapping("/deleteErpPurchaseOrder")
    @Log(businessType= EnumBusinessType.PURCHASE, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除采购订单信息")
    public ResponseResult<Integer> deleteErpPurchaseOrder(HttpServletRequest request, @RequestParam Integer id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (id == null) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.deleteErpPurchaseOrder(userInfo,id);
    }

    @GetMapping("/countErpPurchaseOrderStatu")
    @ApiOperation(value = "统计采购订单状态信息")
    public ResponseResult<OrderStatusCountDTO> countErpPurchaseOrderStatu(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return purchaseOrderService.countErpPurchaseOrderStatu(userInfo.getOrgId());
    }

    @GetMapping("/getErpPurchaseOrderInfoById")
    @ApiOperation(value = "根据采购订单合同id获取采购订单信息")
    public ResponseResult<List<ErpPurchaseOrderInfoDetailDTO>> getErpPurchaseOrderByContractNo(HttpServletRequest request, Integer id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return purchaseOrderService.getErpPurchaseOrderById(userInfo,id);
    }

    @GetMapping("/getErpPurchaseOrderByContractNoList")
    @ApiOperation(value = "获取当前用户所在公司及其子公司关联的采购订单列表")
    public ResponseResult<List<ErpPucharseOrderBoxDTO>> getErpPurchaseOrderContractNoList(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userInfoFeignClient
                .simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
                .getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        // String sysToken = request.getHeader("sysToken");
        return purchaseOrderService.getPucharseOrderBox(userInfo);
    }

    @GetMapping("/getErpPurchaseOrderExcute")
    @ApiOperation(value = "获取采购订单执行信息")
    public ResponseResult<ErpPurchaseOrderExcuteDTO> getErpPurchaseOrderExcute(Integer id){
        if (id == null) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseOrderService.getErpPurchaseOrderExcute(id);
    }


}


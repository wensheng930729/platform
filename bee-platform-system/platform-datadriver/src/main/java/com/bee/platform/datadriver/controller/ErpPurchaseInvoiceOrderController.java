package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpPurchaseInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderInfoDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrder;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.*;
import com.bee.platform.datadriver.service.ErpPurchaseInvoiceOrderService;
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

import org.springframework.stereotype.Controller;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 采购发票 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpPurchaseInvoiceOrder", tags = "erp采购发票相关接口")
@RequestMapping("/erpPurchaseInvoiceOrder")
public class ErpPurchaseInvoiceOrderController {

    @Autowired
    private ErpPurchaseInvoiceOrderService purchaseInvoiceOrderService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/listPurchaseInvoiceOrder")
    @ApiOperation(value = "分页查询采购发票列表")
    public ResponseResult<List<ErpPurchaseInvoiceOrderInfoDTO>> listPurchaseInvoiceOrder(HttpServletRequest request, @RequestBody InvoiceOrderQueryRQ rq , Page page){
        /*String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        if (StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }*/
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (Objects.isNull(companyId) && Objects.isNull(rq.getCompany())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpPurchaseInvoiceOrderInfoDTO> list = purchaseInvoiceOrderService.listPurchaseInvoiceOrder(pagination,rq,companyId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/getPurchaseInvoiceOrder")
    @ApiOperation(value = "获取采购发票信息")
    public ResponseResult<ErpPurchaseInvoiceOrder> getPurchaseInvoiceOrder(HttpServletRequest request, String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseInvoiceOrderService.getPurchaseInvoiceOrder(id);
    }

    @PostMapping("/deletePurchaseInvoiceOrder")
    @Log(businessType= EnumBusinessType.PURCHASE_INVOICE, operateType = OperateType.EDIT)
    @ApiOperation(value = "删除采购发票信息")
    public ResponseResult<Integer> deletePurchaseInvoiceOrder(HttpServletRequest request, @RequestBody OrderDeleteRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseInvoiceOrderService.deletePurchaseInvoiceOrder(userInfo,rq);
    }

    @PostMapping("/savePurchaseInvoiceOrder")
    @Log(businessType= EnumBusinessType.PURCHASE_INVOICE, operateType = OperateType.EDIT)
    @ApiOperation(value = "保存采购发票")
    public ResponseResult<Integer> savePurchaseInvoiceOrder(HttpServletRequest request, @RequestBody ErpPurchaseInvoiceOrderSaveRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return purchaseInvoiceOrderService.savePurchaseInvoiceOrder(userInfo,rq);
    }
}


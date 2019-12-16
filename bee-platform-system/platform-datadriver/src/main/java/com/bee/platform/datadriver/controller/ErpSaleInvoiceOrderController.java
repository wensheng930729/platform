package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderInfoDTO;
import com.bee.platform.datadriver.dto.ErpSaleInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrder;
import com.bee.platform.datadriver.entity.ErpSaleInvoiceOrder;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.*;
import com.bee.platform.datadriver.service.ErpSaleInvoiceOrderService;
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
 * 销售发票 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpSaleInvoiceOrder", tags = "erp销售发票相关接口")
@RequestMapping("/erpSaleInvoiceOrder")
public class ErpSaleInvoiceOrderController {

    @Autowired
    private ErpSaleInvoiceOrderService saleInvoiceOrderService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/listSaleInvoiceOrder")
    @ApiOperation(value = "分页查询销售订单列表")
    public ResponseResult<List<ErpSaleInvoiceOrderInfoDTO>> listSaleInvoiceOrder(HttpServletRequest request, @RequestBody InvoiceOrderQueryRQ rq , Page page){
        /*String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        if (StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }*/
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (Objects.isNull(companyId) && Objects.isNull(rq.getCompany())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ErpSaleInvoiceOrderInfoDTO> list = saleInvoiceOrderService.listSaleInvoiceOrder(pagination,rq,companyId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @GetMapping("/getSaleInvoiceOrder")
    @ApiOperation(value = "获取销售发票信息")
    public ResponseResult<ErpSaleInvoiceOrder> getSaleInvoiceOrder(HttpServletRequest request, String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceOrderService.getSaleInvoiceOrder(id);
    }

    @PostMapping("/deleteSaleInvoiceOrder")
    @Log(businessType= EnumBusinessType.SALE_INVOICE, operateType = OperateType.EDIT)
    @ApiOperation(value = "删除销售发票信息")
    public ResponseResult<Integer> deleteSaleInvoiceOrder(HttpServletRequest request, @RequestBody OrderDeleteRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceOrderService.deleteSaleInvoiceOrder(userInfo,rq);
    }

    @PostMapping("/saveSaleInvoiceOrder")
    @Log(businessType= EnumBusinessType.SALE_INVOICE, operateType = OperateType.EDIT)
    @ApiOperation(value = "保存销售发票")
    public ResponseResult<Integer> saveSaleInvoiceOrder(HttpServletRequest request, @RequestBody ErpSaleInvoiceOrderSaveRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceOrderService.saveSaleInvoiceOrder(userInfo,rq);
    }
}


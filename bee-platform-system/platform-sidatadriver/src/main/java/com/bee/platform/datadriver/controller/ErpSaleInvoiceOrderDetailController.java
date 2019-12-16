package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpInvoiceOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrderDetail;
import com.bee.platform.datadriver.entity.ErpSaleInvoiceOrderDetail;
import com.bee.platform.datadriver.rq.ErpInvoiceOrderDetailRQ;
import com.bee.platform.datadriver.rq.ErpSaleInvoiceOrderSaveRQ;
import com.bee.platform.datadriver.service.ErpSaleInvoiceOrderDetailService;
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
 * 销售发票明细 前端控制器
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpSaleInvoiceOrderDetail", tags = "erp销售发票明细相关接口")
@RequestMapping("/erpSaleInvoiceOrderDetail")
public class ErpSaleInvoiceOrderDetailController {

    @Autowired
    private ErpSaleInvoiceOrderDetailService saleInvoiceOrderDetailService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @GetMapping("/listSaleInvoiceOrderDetail")
    @ApiOperation(value = "获取销售发票明细信息")
    public ResponseResult<List<ErpInvoiceOrderDetailDTO>> listSaleInvoiceOrderDetail(String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceOrderDetailService.listSaleInvoiceOrderDetail(id);
    }

    @GetMapping("/deleteSaleInvoiceOrderDetail")
    @ApiOperation(value = "删除销售发票明细信息")
    public ResponseResult<ResCodeEnum> deleteSaleInvoiceOrderDetail(HttpServletRequest request, String id){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return saleInvoiceOrderDetailService.deleteSaleInvoiceOrderDetail(userInfo, id);
    }

    @PostMapping("/saveSaleInvoiceOrderDetail")
    @ApiOperation(value = "保存采购发票明细发票")
    public ResponseResult<ResCodeEnum> saveSaleInvoiceOrderDetail(HttpServletRequest request, @RequestBody ErpInvoiceOrderDetailRQ rq){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceOrderDetailService.saveSaleInvoiceOrderDetail(userInfo,rq);
    }
}


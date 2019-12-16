package com.bee.platform.dinas.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dto.SaleInvoiceDTO;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SaleInvoiceListRQ;
import com.bee.platform.dinas.datadriver.rq.SaleInvoiceRQ;
import com.bee.platform.dinas.datadriver.service.DinasSaleInvoiceService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.bee.platform.dinas.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 销售发票 前端控制器
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Slf4j
@RestController
@RequestMapping("/dinasSaleInvoice")
@Api(value = "DinasSaleInvoiceController", tags = "砂石销售发票相关接口")
public class DinasSaleInvoiceController {

    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @Autowired
    private DinasSaleInvoiceService saleInvoiceService;

    @PostMapping("/add")
    @ApiOperation(value = "添加销售发票")
    @Log(businessType= EnumBusinessType.SALE_INVOICE, operateType = OperateType.ADD)
    public ResponseResult<Integer> add(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SaleInvoiceRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceService.add(userInfo, rq);
    }

    @PostMapping("/update")
    @ApiOperation(value = "编辑销售发票")
    @Log(businessType= EnumBusinessType.SALE_INVOICE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> update(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid SaleInvoiceRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceService.update(userInfo, rq);
    }

    @GetMapping("/get")
    @ApiOperation(value = "查询销售发票详情")
    public ResponseResult<SaleInvoiceDTO> get(@RequestParam Integer id) {
        return saleInvoiceService.findInfo(id);
    }

    @PostMapping("/delete")
    @ApiOperation(value = "批量删除销售发票")
    @Log(businessType= EnumBusinessType.SALE_INVOICE, operateType = OperateType.DELETE, isBatch = true)
    public ResponseResult batchDelete(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SaleBatchDeleteRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleInvoiceService.batchDelete(userInfo, rq);
    }

    @PostMapping("/list")
    @ApiOperation(value = "查询销售发票列表")
    public ResponseResult<List<SaleInvoiceDTO>> findSaleOrders(@RequestHeader("sysToken") String sysToken, @RequestBody SaleInvoiceListRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return saleInvoiceService.findList(userInfo, rq, pagination);
    }

}


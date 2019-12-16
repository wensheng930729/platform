package com.bee.platform.dinas.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dto.DinasSalePaymentDTO;
import com.bee.platform.dinas.datadriver.dto.SalePaymentListDTO;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SalePaymentListRQ;
import com.bee.platform.dinas.datadriver.rq.SalePaymentRQ;
import com.bee.platform.dinas.datadriver.service.DinasSalePaymentService;
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
 * 销售回款 前端控制器
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Slf4j
@RestController
@RequestMapping("/dinasSalePayment")
@Api(value = "DinasSalePaymentController", tags = "砂石销售回款相关接口")
public class DinasSalePaymentController {

    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @Autowired
    private DinasSalePaymentService salePaymentService;

    @PostMapping("/add")
    @ApiOperation(value = "添加销售回款")
    @Log(businessType= EnumBusinessType.SALE_PAYMENT, operateType = OperateType.ADD)
    public ResponseResult<Integer> add(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SalePaymentRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return salePaymentService.add(userInfo, rq);
    }

    @PostMapping("/update")
    @ApiOperation(value = "编辑销售回款")
    @Log(businessType= EnumBusinessType.SALE_PAYMENT, operateType = OperateType.EDIT)
    public ResponseResult<Integer> update(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid SalePaymentRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return salePaymentService.update(userInfo, rq);
    }

    @GetMapping("/get")
    @ApiOperation(value = "查询销售回款详情")
    public ResponseResult<DinasSalePaymentDTO> get(@RequestParam Integer id) {
        return salePaymentService.findInfo(id);
    }

    @PostMapping("/delete")
    @ApiOperation(value = "批量删除销售回款")
    @Log(businessType= EnumBusinessType.SALE_PAYMENT, operateType = OperateType.DELETE, isBatch = true)
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
        return salePaymentService.batchDelete(userInfo, rq);
    }

    @PostMapping("/list")
    @ApiOperation(value = "查询销售回款列表")
    public ResponseResult<List<SalePaymentListDTO>> findSaleOrders(@RequestHeader("sysToken") String sysToken, @RequestBody SalePaymentListRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return salePaymentService.findList(userInfo, rq, pagination);
    }

}


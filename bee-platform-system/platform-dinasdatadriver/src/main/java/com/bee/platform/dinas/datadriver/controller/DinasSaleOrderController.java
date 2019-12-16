package com.bee.platform.dinas.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dto.SaleAdjustDetailDTO;
import com.bee.platform.dinas.datadriver.dto.SaleOrderDTO;
import com.bee.platform.dinas.datadriver.dto.SaleOrderDetailDTO;
import com.bee.platform.dinas.datadriver.dto.SaleOrderListDTO;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.SaleAdjustRQ;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SaleOrderListRQ;
import com.bee.platform.dinas.datadriver.rq.SaleOrderRQ;
import com.bee.platform.dinas.datadriver.service.DinasSaleAdjustService;
import com.bee.platform.dinas.datadriver.service.DinasSaleOrderService;
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
 * 销售合同 前端控制器
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
@Slf4j
@RestController
@RequestMapping("/dinasSaleOrder")
@Api(value = "DinasSaleOrderController", tags = "砂石销售合同相关接口")
public class DinasSaleOrderController {

    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @Autowired
    private DinasSaleOrderService saleOrderService;

    @Autowired
    private DinasSaleAdjustService saleAdjustService;

    @PostMapping("/add")
    @ApiOperation(value = "添加销售合同")
    @Log(businessType= EnumBusinessType.SALE, operateType = OperateType.ADD)
    public ResponseResult<Integer> add(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid SaleOrderRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleOrderService.add(userInfo, rq);
    }

    @PostMapping("/update")
    @ApiOperation(value = "编辑销售合同")
    @Log(businessType= EnumBusinessType.SALE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> update(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid SaleOrderRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleOrderService.update(userInfo, rq);
    }

    @GetMapping("/get")
    @ApiOperation(value = "查询销售合同详情")
    public ResponseResult<SaleOrderDTO> get(@RequestParam Integer id) {
        return saleOrderService.findSaleOrderInfo(id);
    }

    @PostMapping("/delete")
    @ApiOperation(value = "批量删除销售合同")
    @Log(businessType= EnumBusinessType.SALE, operateType = OperateType.DELETE, isBatch = true)
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
        return saleOrderService.batchDelete(userInfo, rq);
    }

    @PostMapping("/list")
    @ApiOperation(value = "查询销售合同列表")
    public ResponseResult<List<SaleOrderListDTO>> findSaleOrders(@RequestHeader("sysToken") String sysToken, @RequestBody SaleOrderListRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return saleOrderService.findSaleOrders(userInfo, rq, pagination);
    }

    @PostMapping("/addAdjust")
    @ApiOperation(value = "添加销售合同调价函")
    public ResponseResult addAdjust(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid SaleAdjustRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数不能为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleAdjustService.add(userInfo, rq);
    }

    @GetMapping("/getAdjustDetails")
    @ApiOperation(value = "根据调价函id查询调价函详情")
    public ResponseResult<List<SaleAdjustDetailDTO>> getAdjustDetails(@RequestParam Integer id) {
        return saleAdjustService.getAdjustDetails(id);
    }

    @GetMapping("/getOrders")
    @ApiOperation(value = "获取当前用户所在公司的销售合同列表")
    public ResponseResult<List<SaleOrderDTO>> getOrders(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleOrderService.findOrdersByCompanyId(userInfo.getOrgId());
    }

    @GetMapping("/getProducts")
    @ApiOperation(value = "根据销售合同id查询当前合同下的产品")
    public ResponseResult<List<SaleOrderDetailDTO>> findProductsById(Integer id) {
        return saleOrderService.findProductsById(id);
    }

    @GetMapping("/getProductSpecs")
    @ApiOperation(value = "根据销售合同id及产品id查询当前产品下的规格")
    public ResponseResult<List<SaleOrderDetailDTO>> findSpecsByProductId(@RequestParam Integer orderId, @RequestParam Integer productId) {
        return saleOrderService.findSpecsByProductId(orderId, productId);
    }

    @GetMapping("/getPriceByProductSpec")
    @ApiOperation(value = "根据产品id及规格id查询调价前价格")
    public ResponseResult<SaleOrderDetailDTO> getPriceByProductSpec(@RequestParam Integer orderId, @RequestParam Integer productId, @RequestParam Integer productSpecId){
        if (orderId == null || productId == null || productSpecId == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return saleOrderService.getPriceByProductSpec(orderId,productId,productSpecId);
    }

}


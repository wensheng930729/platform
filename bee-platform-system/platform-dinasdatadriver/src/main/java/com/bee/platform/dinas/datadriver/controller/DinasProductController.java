package com.bee.platform.dinas.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.DinasProductAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasProductQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasProductUpdateRQ;
import com.bee.platform.dinas.datadriver.service.DinasProductService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.bee.platform.dinas.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 产品表 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@RestController
@RequestMapping("/dinasProduct")
@Api(value = "DinasProductController", tags = "砂石产品相关接口")
public class DinasProductController {

    @Autowired
    private DinasProductService productService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/add")
    @ApiOperation(value = "增加")
    @Log(businessType = EnumBusinessType.PRODUCT, operateType = OperateType.ADD)
    public ResponseResult<Integer> addProduct(@RequestHeader("sysToken") String sysToken, @Valid @RequestBody DinasProductAddRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.addProduct(userInfo, rq);
    }

    @PostMapping("/delete")
    @ApiOperation(value = "删除")
    @Log(businessType = EnumBusinessType.PRODUCT, operateType = OperateType.DELETE, isBatch = true)
    public ResponseResult<List<Integer>> deleteProduct(@RequestHeader("sysToken") String sysToken, @RequestBody List<Integer> ids) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (CollectionUtils.isEmpty(ids)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_SELECT_RECORD);
        }
        return productService.deleteProduct(userInfo, ids);
    }

    @PostMapping("/update")
    @ApiOperation(value = "编辑")
    @Log(businessType = EnumBusinessType.PRODUCT, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateProduct(@RequestHeader("sysToken") String sysToken, @Valid @RequestBody DinasProductUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.updateProduct(userInfo, rq);
    }

    @GetMapping("/switch/{id}/{status}")
    @ApiOperation(value = "切换状态")
    @Log(businessType = EnumBusinessType.PRODUCT, operateType = OperateType.EDIT)
    public ResponseResult<Integer> switchProduct(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id, @PathVariable("status") Integer status) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.switchProduct(userInfo, id, status);
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "详情")
    public ResponseResult<DinasProductDetailDTO> getProductDetail(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getProductDetail(id);
    }

    @PostMapping("/list")
    @ApiOperation(value = "列表")
    public ResponseResult<List<DinasProductList2DTO>> getProductList(@RequestHeader("sysToken") String sysToken, Page page, @RequestBody DinasProductQueryRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer orgId = userInfo.getOrgId();
        return productService.getProductList(orgId, page, rq);
    }

    @GetMapping("/relateAll")
    @ApiOperation(value = "客户可关联产品-产品和规格")
    public ResponseResult<List<DinasProductSpecAllocateDTO>> getRelateProductAll(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer orgId = userInfo.getOrgId();
        return productService.getRelateProductAll(orgId);
    }

    @GetMapping("/relateProductSpec/{customerId}")
    @ApiOperation(value = "客户已关联产品-产品和规格")
    public ResponseResult<List<DinasProductSpecAllocateDTO>> getRelateProductCustomer(@PathVariable("customerId") Integer customerId) {
        return productService.getRelateProductCustomer(customerId);
    }

    @GetMapping("/relateProduct")
    @ApiOperation(value = "查询当前用户企业下产品")
    public ResponseResult<List<DinasProductBoxDTO>> getCompanyProduct(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer orgId = userInfo.getOrgId();
        return productService.getCompanyProduct(orgId);
    }

    @GetMapping("/relateProduct/{customerId}")
    @ApiOperation(value = "客户关联产品-不含规格")
    public ResponseResult<List<DinasProductBoxDTO>> getCustomerProduct(@RequestHeader("sysToken") String sysToken, @PathVariable("customerId") Integer customerId) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getCustomerProduct(customerId);
    }

    @GetMapping("/relateSpec/{customerId}/{productId}")
    @ApiOperation(value = "客户关联产品中某个产品的规格")
    public ResponseResult<List<DinasProductSpecBoxDTO>> getCustomerProductSpec(@RequestHeader("sysToken") String sysToken,
                                                                               @PathVariable("customerId") Integer customerId,
                                                                               @PathVariable("productId") Integer productId) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getCustomerProductSpec(customerId, productId);
    }

}


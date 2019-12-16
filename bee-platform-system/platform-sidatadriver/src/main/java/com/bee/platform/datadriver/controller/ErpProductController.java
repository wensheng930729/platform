package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpProductAddRQ;
import com.bee.platform.datadriver.rq.ErpProductListRQ;
import com.bee.platform.datadriver.rq.ErpProductUpdateRQ;
import com.bee.platform.datadriver.service.ErpProductService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 产品档案 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@RequestMapping("/product")
@Api(value = "ErpProduct", tags = "erp产品相关接口")
public class ErpProductController {

    @Autowired
    private ErpProductService productService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/conditional")
    @ApiOperation(value = "列表")
    public ResponseResult<List<ErpProductListDTO>> getProductConditional(@RequestHeader("sysToken") String sysToken, @RequestBody(required = false) ErpProductListRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
//        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
//        if (ObjectUtils.isEmpty(companyId)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.NO_ENTERPRISE_ID);
//        }
        return productService.getProductConditional(rq, page, userInfo.getOrgId());
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "产品详情")
    public ResponseResult<ErpProductDetailDTO> getById(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getById(id);
    }

    @PostMapping("/code")
    @ApiOperation(value = "生成产品编码")
    public ResponseResult<String> generateProductCode(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.generateProductCode(userInfo.getOrgId());
    }

    @PostMapping("/add")
    @ApiOperation(value = "增加")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.ADD)
    public ResponseResult<Integer> addProduct(@RequestHeader("sysToken") String sysToken, @RequestBody ErpProductAddRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.addProduct(rq, userInfo);
    }

    @GetMapping("/delete/{id}")
    @ApiOperation(value = "删除")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.DELETE)
    public ResponseResult<Integer> deleteProduct(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.deleteProduct(id, userInfo);
    }

    @PostMapping("/update")
    @ApiOperation(value = "更新")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateProduct(@RequestHeader("sysToken") String sysToken, @RequestBody ErpProductUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.updateProduct(rq, userInfo);
    }

    @GetMapping("/updateStatus/{id}/{status}")
    @ApiOperation(value = "禁用-启用产品")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateStatus(@RequestHeader("sysToken") String sysToken, Integer id, Integer status) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.updateStatus(id, status, userInfo);
    }

    @GetMapping("/enterpriseProduct")
    @ApiOperation(value = "下拉框树形-当前用户企业查询-产品及批次")
    public ResponseResult<List<ErpProductBoxTreeDTO>> getEnterpriseProduct(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getEnterpriseProduct(userInfo.getOrgId());
    }

    @PostMapping("/enterpriseProductByCategory")
    @ApiOperation(value = "下拉框树形-当前用户企业和分类查询-产品及批次")
    public ResponseResult<List<ErpProductBoxTreeDTO>> getEnterpriseProductByCategory(@RequestHeader("sysToken") String sysToken, @RequestBody(required = false) List<Integer> categoryList) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getEnterpriseProductByCategory(userInfo.getOrgId(), categoryList);
    }

    @PostMapping("/checkItems/{id}")
    @ApiOperation(value = "根据产品id获取-检测属性")
    public ResponseResult<List<ErpProductCheckItemsDTO>> getCheckItems(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getCheckItems(id);
    }

    @GetMapping("/getProductListByCategory")
    @ApiOperation(value = "根据产品分类查询产品列表")
    public ResponseResult<List<ErpProductListByCategoryDTO>> getProductListByCategory(@RequestHeader("sysToken") String sysToken, @RequestParam() Integer category) {
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("获取登录信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        List<ErpProductListByCategoryDTO> dto = productService.getProductListByCategory(userInfo.getOrgId(), category);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}


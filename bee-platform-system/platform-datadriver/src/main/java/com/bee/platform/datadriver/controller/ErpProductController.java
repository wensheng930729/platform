package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpProductBoxDTO;
import com.bee.platform.datadriver.dto.ErpProductCategoryCheckItemsDTO;
import com.bee.platform.datadriver.dto.ErpProductDetailDTO;
import com.bee.platform.datadriver.dto.ErpProductListDTO;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 产品档案 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@RestController
@RequestMapping("/product")
@Api(value = "ErpProduct", tags = "erp产品相关接口")
public class ErpProductController {

    @Autowired
    private ErpProductService productService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

    @PostMapping("/conditional")
    @ApiOperation(value = "查询erp产品列表")
    public ResponseResult<List<ErpProductListDTO>> getProductConditional(HttpServletRequest request, @RequestBody(required = false) ErpProductListRQ rq, Page page) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (ObjectUtils.isEmpty(companyId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_ENTERPRISE_ID);
        }
        return productService.getProductConditional(rq, page, companyId);
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "根据id查询erp产品详情")
    public ResponseResult<ErpProductDetailDTO> getById(HttpServletRequest request,@PathVariable("id") Integer id) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getById(id);
    }

    @PostMapping("/add")
    @ApiOperation(value = "增加erp产品")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.ADD)
    public ResponseResult<Integer> addProduct(HttpServletRequest request, @RequestBody ErpProductAddRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.addProduct(rq, userInfo);
    }

    @GetMapping("/delete/{id}")
    @ApiOperation(value = "删除erp产品")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.DELETE)
    public ResponseResult<Integer> deleteProduct(HttpServletRequest request,@PathVariable("id") Integer id) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.deleteProduct(id, userInfo);
    }

    @PostMapping("/update")
    @ApiOperation(value = "更新erp产品")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateProduct(HttpServletRequest request,@RequestBody ErpProductUpdateRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.updateProduct(rq, userInfo);
    }

    @GetMapping("/updateStatus/{id}/{status}")
    @ApiOperation(value = "禁用--启用产品")
    @Log(businessType = EnumBusinessType.PRODUCT_ARCHIVE, operateType = OperateType.EDIT)
    public  ResponseResult<Integer> updateStatus(HttpServletRequest request,Integer id, Integer status){
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.updateStatus(id, status, userInfo);
    }

    @GetMapping("/enterpriseProduct")
    @ApiOperation(value = "当前用户所在企业的产品-下拉框")
    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseProduct(HttpServletRequest request) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getEnterpriseProduct(userInfo);
    }

    @GetMapping("/enterpriseSubProduct")
    @ApiOperation(value = "当前用户所在企业及-子企业的产品")
    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseSubProduct(HttpServletRequest request){
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getEnterpriseSubProduct(userInfo);
    }

    @GetMapping("/groupProduct")
    @ApiOperation(value = "当前用户所在企业及-集团下所有公司的产品")
    public ResponseResult<List<ErpProductBoxDTO>> getEnterpriseGroupProduct(HttpServletRequest request){
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getEnterpriseGroupProduct(userInfo);
    }

    @PostMapping("/checkItems/{id}")
    @ApiOperation(value = "根据产品id获取-该产品启用的检测属性")
    public ResponseResult<List<ErpProductCategoryCheckItemsDTO>> getCheckItems(HttpServletRequest request,@PathVariable("id") Integer id){
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productService.getCheckItems(id);
    }
}


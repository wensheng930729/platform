package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpProductCategoryDTO;
import com.bee.platform.datadriver.service.ErpProductCategoryService;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 产品类别 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@RestController
@RequestMapping("/productCategory")
@Api(value = "ErpProductCategory", tags = "erp产品分类相关接口")
public class ErpProductCategoryController {

    @Autowired
    private ErpProductCategoryService productCategoryService;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;

//    @PostMapping("/add")
//    @ApiOperation(value = "添加erp产品分类")
//    @Log(businessType = EnumBusinessType.PRODUCT_CATEGORY, operateType = OperateType.ADD)
//    public ResponseResult<Integer> addCategory(HttpServletRequest request, @RequestBody ErpProductCategoryAddRQ rq) {
//        String sysToken = request.getHeader("sysToken");
//        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
//        if (ObjectUtils.isEmpty(userInfo)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
//        }
//        return productCategoryService.addCategory(rq, userInfo);
//    }
//
//    @GetMapping("/delete/{id}")
//    @ApiOperation(value = "删除erp产品分类")
//    @Log(businessType = EnumBusinessType.PRODUCT_CATEGORY, operateType = OperateType.DELETE)
//    public ResponseResult<Integer> deleteCategory(HttpServletRequest request,@PathVariable("id") Integer id) {
//        String sysToken = request.getHeader("sysToken");
//        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
//        if (ObjectUtils.isEmpty(userInfo)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
//        }
//        return productCategoryService.deleteCategory(id, userInfo);
//    }
//
//    @PostMapping("/update")
//    @ApiOperation(value = "更新erp产品分类")
//    @Log(businessType =EnumBusinessType.PRODUCT_CATEGORY, operateType = OperateType.EDIT)
//    public ResponseResult<Integer> updateCategory(HttpServletRequest request, @RequestBody ErpProductCategoryUpdateRQ rq) {
//        String sysToken = request.getHeader("sysToken");
//        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
//        if (ObjectUtils.isEmpty(userInfo)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
//        }
//        return productCategoryService.updateCategory(rq, userInfo);
//    }
//
//    @GetMapping("/updateStatus/{id}/{status}")
//    @ApiOperation(value = "禁用--启用产品分类")
//    @Log(businessType = EnumBusinessType.PRODUCT_CATEGORY, operateType = OperateType.EDIT)
//    public ResponseResult<Integer> updateStatus(HttpServletRequest request, Integer id, Integer status) {
//        String sysToken = request.getHeader("sysToken");
//        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
//        if (ObjectUtils.isEmpty(userInfo)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
//        }
//        return productCategoryService.updateStatus(id, status, userInfo);
//    }
//
//
//    @PostMapping("/conditional")
//    @ApiOperation(value = "查询erp产品分类列表")
//    public ResponseResult getCategoryConditional(HttpServletRequest request, @RequestBody(required = false) ErpProductCategoryListRQ rq, Page page) {
//        String sysToken = request.getHeader("sysToken");
//        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
//        if (ObjectUtils.isEmpty(userInfo)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
//        }
//        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
//        if (ObjectUtils.isEmpty(companyId)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.NO_ENTERPRISE_ID);
//        }
//        return productCategoryService.getCategoryConditional(rq, page, companyId);
//    }
//
//    @GetMapping("/conditional/{id}")
//    @ApiOperation(value = "通过id获取 erp产品分类详情")
//    public ResponseResult getById(@PathVariable("id") Integer id) {
//        return productCategoryService.getById(id);
//    }

    @GetMapping("/category")
    @ApiOperation(value = "获取当前登录人所在企业所有的产品分类(下拉框)")
    public ResponseResult<List<ErpProductCategoryDTO>> getCategory(HttpServletRequest request) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return productCategoryService.getCategory();
    }

//    @GetMapping("/checkItems/{id}")
//    @ApiOperation(value = "根据产品类别id获取该企业下产品分类的检测属性")
//    public ResponseResult<List<ErpProductCheckItemsDTO>> getCheckItems(HttpServletRequest request, @PathVariable("id") Integer id) {
//        String sysToken = request.getHeader("sysToken");
//        AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
//        if (ObjectUtils.isEmpty(userInfo)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
//        }
//        return productCategoryService.getCheckItems(id);
//    }


}


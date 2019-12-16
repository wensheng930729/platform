package com.bee.platform.customer.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.customer.dto.ErpCustomerCategoryBoxDTO;
import com.bee.platform.customer.dto.ErpCustomerCategoryListDTO;
import com.bee.platform.customer.rq.ErpCustomerCategoryAddRQ;
import com.bee.platform.customer.rq.ErpCustomerCategorySelectRQ;
import com.bee.platform.customer.rq.ErpCustomerCategoryUpdateRQ;
import com.bee.platform.customer.service.ErpCustomerCategoryService;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 客户分类 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/erpCustomerCategory")
@Api(value = "ErpCustomerCategory", tags = "erp客户分类相关的接口")
public class ErpCustomerCategoryController {

    @Autowired
    private ErpCustomerCategoryService customerCategoryService;
    @Autowired
    private AuthUserFeignClient userFeignClient;

    @ApiOperation(value = "客户分类添加", notes = "客户分类添加")
    @PostMapping(value = "/add")
    @Log(businessType = EnumBusinessType.CUSTOMER_CATEGORY, operateType = OperateType.ADD)
    public ResponseResult<Integer> addCustomerCategory(HttpServletRequest request, @RequestBody @Validated ErpCustomerCategoryAddRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerCategoryService.addCustomerCategory(rq, userInfo);
    }

    @ApiOperation(value = "删除客户分类", notes = "删除客户分类")
    @GetMapping(value = "/delete/{id}")
    @Log(businessType = EnumBusinessType.CUSTOMER_CATEGORY, operateType = OperateType.DELETE)
    public ResponseResult<Integer> deleteCustomerCategory(HttpServletRequest request, @PathVariable("id") Integer id) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerCategoryService.deleteCustomerCategory(id,userInfo);
    }

    @ApiOperation(value = "修改客户分类", notes = "修改客户分类")
    @PostMapping(value = "/update")
    @Log(businessType = EnumBusinessType.CUSTOMER_CATEGORY, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateCustomerCategory(HttpServletRequest request, @RequestBody @Validated ErpCustomerCategoryUpdateRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerCategoryService.updateCustomerCategory(rq, userInfo);
    }

    @ApiOperation(value = "客户启用--禁用", notes = "客户启用--禁用")
    @GetMapping(value = "/updateStatus/{id}/{status}")
    @Log(businessType = EnumBusinessType.CUSTOMER_CATEGORY, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateSatatus(HttpServletRequest request,@PathVariable("id") Integer id, @PathVariable("status") Integer status) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerCategoryService.updateSatatus(id, status,userInfo);
    }

    @ApiOperation(value = "查询客户分类列表", notes = "查询客户分类列表")
    @PostMapping(value = "/list")
    public ResponseResult<List<ErpCustomerCategoryListDTO>> getList(HttpServletRequest request, @RequestBody(required = false) ErpCustomerCategorySelectRQ rq, Page page) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (ObjectUtils.isEmpty(companyId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_ENTERPRISE_ID);
        }
        return customerCategoryService.getList(rq, companyId, page);
    }

    @ApiOperation(value = "通过分类类型-查询企业下的二级客户分类-下拉框使用)", notes = "通过分类类型-查询企业下的二级客户分类-下拉框使用")
    @GetMapping(value = "/secondCategory")
    public ResponseResult<List<ErpCustomerCategoryBoxDTO>> getEnterpriseTwoCategories(HttpServletRequest request, String pcode) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerCategoryService.getEnterpriseTwoCategories(pcode, userInfo);
    }

    @ApiOperation(value = "通过登录用户企业下的二级客户分类-下拉框使用", notes = "通过登录用户企业下的二级客户分类-下拉框使用")
    @GetMapping(value = "/user/secondCategory")
    public ResponseResult<List<ErpCustomerCategoryBoxDTO>> getUserTwoCategories(HttpServletRequest request){
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return customerCategoryService.getUserTwoCategories(userInfo);
    }

}


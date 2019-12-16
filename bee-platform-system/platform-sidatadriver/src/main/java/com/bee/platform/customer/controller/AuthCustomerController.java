package com.bee.platform.customer.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.customer.dto.AuthCustomerBoxDto;
import com.bee.platform.customer.dto.AuthCustomerDetailDTO;
import com.bee.platform.customer.dto.CustomerFirstCategoryBoxDto;
import com.bee.platform.customer.rq.AuthCustomerAddRQ;
import com.bee.platform.customer.rq.AuthCustomerSelectRQ;
import com.bee.platform.customer.rq.AuthCustomerUpdateRQ;
import com.bee.platform.customer.service.AuthCustomerService;
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
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author hongchuan.He
 * @since 2019-05-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authCustomer", tags = "erp客户相关的接口")
@RequestMapping("/authCustomer")
public class AuthCustomerController {

    @Autowired
    private AuthCustomerService authCustomerService;
    @Autowired
    private AuthUserFeignClient userFeignClient;

    @ApiOperation(value = "客户添加", notes = "客户添加")
    @PostMapping(value = "/add")
    @Log(businessType = EnumBusinessType.CUSTOMER_PROFILE, operateType = OperateType.ADD)
    public ResponseResult<Integer> addAuthCustomer(HttpServletRequest request, @RequestBody @Valid AuthCustomerAddRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return authCustomerService.addCustomer(rq, userInfo);
    }

    @ApiOperation(value = "客户删除", notes = "客户删除")
    @GetMapping(value = "/delete/{id}")
    @Log(businessType = EnumBusinessType.CUSTOMER_PROFILE, operateType = OperateType.DELETE)
    public ResponseResult<Integer> deleteAuthCustomerById(HttpServletRequest request, @PathVariable("id") Integer id) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_ID_EMPTY);
        }
        return authCustomerService.deleteCustomer(id, userInfo);
    }

    @ApiOperation(value = "更新客户信息", notes = "更新客户信息")
    @PostMapping(value = "/update")
    @Log(businessType = EnumBusinessType.CUSTOMER_PROFILE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> updateCustomer(HttpServletRequest request, @RequestBody() @Valid AuthCustomerUpdateRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return authCustomerService.updateCustomer(rq, userInfo);
    }

    @ApiOperation(value = "客户启用禁用", notes = "客户启用禁用")
    @GetMapping(value = "/updateStatus/{id}/{status}")
    @Log(businessType = EnumBusinessType.CUSTOMER_PROFILE, operateType = OperateType.EDIT)
    public ResponseResult<Integer> upDateByAuthCustomer(HttpServletRequest request, @PathVariable("id") Integer id, @PathVariable("status") Integer status) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return authCustomerService.updateStatus(id, status, userInfo);
    }

    @ApiOperation(value = "根据客户id查询客户详情", notes = "根据客户id查询客户详情")
    @PostMapping(value = "/detail/{id}")
    public ResponseResult<AuthCustomerDetailDTO> getCustomerDetail(HttpServletRequest request,@PathVariable("id") Integer customerId) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return authCustomerService.getCustomerDetail(customerId);
    }

    @ApiOperation(value = "客户查询", notes = "客户查询")
    @PostMapping(value = "/list")
    public ResponseResult getAuthCustomerById(HttpServletRequest request, @RequestBody @Validated AuthCustomerSelectRQ rq, Page page) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
        if (ObjectUtils.isEmpty(companyId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_ENTERPRISE_ID);
        }

        return authCustomerService.getList(rq, page, companyId);
    }

    @ApiOperation(value = "下拉框-根据token和pcode查询企业及子企业", notes = "下拉框-根据token和pcode查询企业及子企业")
    @GetMapping(value = "/enterprise/customer")
    public ResponseResult<List<AuthCustomerBoxDto>> getEnterpriseCustomer(HttpServletRequest request, String pcode) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return authCustomerService.getEnterpriseCustomer(pcode, userInfo);
    }

    @ApiOperation(value = "下拉框-根据企业id查询", notes = "下拉框-根据企业id查询")
    @GetMapping(value = "/customer/enterprise/{orgId}")
    public ResponseResult<List<AuthCustomerBoxDto>> getCustomerByOrgId(@PathVariable("orgId") Integer orgId) {
        return authCustomerService.getCustomerByOrgId(orgId);
    }

    @ApiOperation(value = "从erp码表查询客户一级分类", notes = "从erp码表查询客户一级分类")
    @GetMapping(value = "/customerFirstCategory")
    public ResponseResult<List<CustomerFirstCategoryBoxDto>> getCustomerFirstCatory(HttpServletRequest request) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = userFeignClient.simpleUserInfo(sysToken).getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return authCustomerService.getCustomerFirstCatory();
    }
}


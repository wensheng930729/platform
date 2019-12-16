package com.bee.platform.user.authority.controller;


import java.util.List;
import java.util.Objects;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.dto.AuthPlatformUserDto;
import com.bee.platform.user.authority.dto.AuthPlatformUserEnterpriseDTO;
import com.bee.platform.user.authority.dto.AuthUserEnterpriseDetailDto;
import com.bee.platform.user.authority.dto.AuthUserEnterprisesDto;
import com.bee.platform.user.authority.rq.AuthPlatformUserRQ;
import com.bee.platform.user.authority.rq.AuthPlatformUserSelectRQ;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;

/**
 * <p>
 * 企业与用户中间表 前端控制器
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@RestController
@RequestMapping("/authPlatformUserEnterprise")
@Api(value = "新权限：企业用户关联管理表",tags = "新权限：企业用户关联管理表")
public class AuthPlatformUserEnterpriseController {

    @Autowired
    private AuthPlatformUserEnterpriseService userEnterpriseService;
    @Autowired
    private AuthPlatformUserService userService;

    @ApiOperation(value = "条件查询企业用户列表", notes = "条件查询企业用户列表")
    @PostMapping("/getList")
    public ResponseResult getList(@RequestHeader("sysToken")String sysToken,
                                  @RequestBody AuthPlatformUserSelectRQ authPlatformUserSelectRQ, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        List<AuthPlatformUserDto> authPlatformUsers = userEnterpriseService
                .getList(sysToken, authPlatformUserSelectRQ, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authPlatformUsers,
                PageUtils.transToPage(pagination));
    }
    
    @ApiOperation(value = "条件查询指定企业用户列表", notes = "条件查询指定企业用户列表")
    @PostMapping("/getListOfEnterprise")
    public ResponseResult getListOfEnterprise(@RequestHeader("sysToken")String sysToken,
                                  @RequestBody AuthPlatformUserSelectRQ authPlatformUserSelectRQ, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Preconditions.checkArgument(authPlatformUserSelectRQ.getEnterpriseId()!= null,"企业id为空");
        List<AuthPlatformUserDto> authPlatformUsers = userEnterpriseService
                .getListOfEnterprise(authPlatformUserSelectRQ, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authPlatformUsers,
                PageUtils.transToPage(pagination));
    }
    
    @ApiOperation(value="用户根据用户查询企业",notes="用户根据用户查询企业")
    @PostMapping("/qureyEnterpriseUser")
    public List<AuthPlatformUserEnterpriseDTO> qureyEnterpriseUser(@RequestParam int userId) {
		return userEnterpriseService.qureyEnterpriseUser(userId);
    }

    @ApiOperation(value="添加企业用户",notes="添加企业用户")
    @PostMapping("/add")
    public ResponseResult add(HttpServletRequest request, @RequestBody @Valid AuthPlatformUserRQ authInterfaceRQs) {
        if (!Validator.isMobile(authInterfaceRQs.getUsername())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        return userEnterpriseService.add(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request), authInterfaceRQs);
    }

    @ApiOperation(value = "更新企业用户", notes = "更新企业用户")
    @ApiImplicitParam(name = "id", value = "接口id", required = true, dataType = "int")
    @PostMapping("/update/{id}")
    public ResponseResult update(HttpServletRequest request, @PathVariable Integer id, @RequestBody @Valid AuthPlatformUserRQ authPlatformUserRQ) {
        return userEnterpriseService.update(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request), id, authPlatformUserRQ);
    }

    @ApiOperation(value = "禁用或启用企业用户", notes = "禁用或启用企业用户")
    @ApiImplicitParams({@ApiImplicitParam(name = "userId", value = "用户id", required = true),
            @ApiImplicitParam(name = "status", value = "用户要更新的状态：0禁用 1启用", required = true)})
    @PostMapping("/update/status/{userId}/{enterpriseId}")
    public ResponseResult updateStatus(HttpServletRequest request, @PathVariable Integer userId,
                                       @PathVariable Integer enterpriseId,
                                       @RequestParam("status") Integer status) {
        if (Objects.isNull(userId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PLATFORM_USER_ID_EMPTY);
        }
        return userEnterpriseService.updateStatus(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request),
                userId, enterpriseId, status);
    }

    @ApiOperation(value = "管理员为用户重置密码", notes = "管理员为用户重置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "用户id", name = "userId", required = true),
            @ApiImplicitParam(value = "重置密码的方式，0为手机号重置，1为邮箱重置", name = "type", defaultValue = "0,1", required = true),
            @ApiImplicitParam(value = "企业id", name = "enterpriseId", required = true)
    })
    @RequestMapping(value = "/resetMemberPassword", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> resetMemberPassword(@RequestHeader("sysToken") String sysToken,
                                                           @RequestParam Integer userId,
                                                           @RequestParam Integer type,
                                                           @RequestParam Integer enterpriseId) {
        if (Objects.isNull(userId) || Objects.isNull(type)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return userEnterpriseService.resetMemberPassword(sysToken, userId, type, enterpriseId);
    }

    @ApiOperation(value = "查询用户所在企业详情", notes = "查询用户所在企业详情")
    @ApiImplicitParam(value = "用户id", name = "userId", required = true)
    @RequestMapping(value = "/info", method = RequestMethod.GET)
    public ResponseResult<AuthUserEnterpriseDetailDto> findUserEnterpriseInfo(@RequestHeader("sysToken") String sysToken,
                                                                              @RequestParam Integer userId) {
        if (Objects.isNull(userId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        AuthUserEnterpriseDetailDto userEnterpriseAndResourceInfo = userEnterpriseService.findUserEnterpriseAndResourceInfo(userInfo, userId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userEnterpriseAndResourceInfo);
    }

    @ApiOperation(value = "根据当前登录人查询企业", notes = "根据当前登录人查询企业")
    @GetMapping(value = "/loginUser/enterprises")
    ResponseResult<List<AuthUserEnterprisesDto>> getUserEnterprises(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo;
        try {
             userInfo = userService.getSelfInfo(sysToken);
            if (ObjectUtils.isEmpty(userInfo)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
            }
        } catch (Exception e) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, Lists.newArrayList());
        }
        return userEnterpriseService.getUserEnterprises(userInfo);
    }
    @ApiOperation(value="内部服务",notes="查询部门及所有下属部门人员id")
    @GetMapping("/findDepartmentIdAndEnterpriseId")
    public ResponseResult<Set<Integer>> findDepartmentIdAndEnterpriseId(@RequestParam List<Integer> departmentId, @RequestParam Integer enterpriseId) {
		return userEnterpriseService.findDepartmentIdAndEnterpriseId(departmentId, enterpriseId);
		
    }
}


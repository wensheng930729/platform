package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthRoleUsedDTO;
import com.bee.platform.user.authority.rq.AuthEnterpriseRoleConfigRQ;
import com.bee.platform.user.authority.rq.RoleRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthFunctionRoleService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.AuthEnterpriseAppFunDTO;
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
import java.util.Set;

/**
 * <p>
 * 角色关联功能 / 功能关联应用  前端控制器
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authFunctionRole", tags = "新权限：erp角色--功能--应用相关接口")
@RequestMapping("/authFunctionRole")
public class AuthFunctionRoleController {

    @Autowired
    private AuthFunctionRoleService functionRoleService;

    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation(value = "角色关联功能 或 功能关联应用")
    @PostMapping(value = "/roleRelationRole")
    public ResponseResult roleRelationRole(@RequestParam() Integer creatorId, @RequestBody @Valid RoleRelationRoleRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        functionRoleService.changeFunctionRole(creatorId, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation(value = "查询所有应用-功能1-功能2--企业权限配置")
    @GetMapping(value = "/get/allFunction")
    ResponseResult<List<AuthEnterpriseAppFunDTO>> getAllFuns() {
        return functionRoleService.getAllFuns();
    }

    @ApiOperation(value = "中台使用--查询当前用户企业下所有应用下的所有功能")
    @GetMapping(value = "/get/AuthEnterprise")
    ResponseResult<List<AuthRoleTreeDTO>> getEnterpriseFuns(HttpServletRequest request){
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return functionRoleService.getEnterpriseFuns(userInfo.getOrgId());
    }

    @ApiOperation(value = "根据查询企业下所有已开通的功能--企业权限配置")
    @GetMapping("/getFuns")
    ResponseResult<List<AuthRoleUsedDTO>> getFunsIds(@RequestParam("enterpriseId") Integer enterpriseId) {
        return functionRoleService.getEnterpriseFunsByEnterpriseId(enterpriseId);
    }

    @ApiOperation(value = "保存企业角色配置的修改")
    @PostMapping("/updateRole")
    ResponseResult changeEnterpriseRole(HttpServletRequest request, @RequestBody @Validated AuthEnterpriseRoleConfigRQ rq) {
        String sysToken = request.getHeader("sysToken");
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return functionRoleService.changeEnterpriseRole(rq, userInfo);
    }
}


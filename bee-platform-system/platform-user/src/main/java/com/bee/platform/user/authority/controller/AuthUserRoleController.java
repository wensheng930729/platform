package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthRoleRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthUserRoleTreeDTO;
import com.bee.platform.user.authority.dto.UserInterfaceUriDTO;
import com.bee.platform.user.authority.rq.AssignPermissionRQ;
import com.bee.platform.user.authority.rq.UserRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthUserRoleService;
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
 * 用户与角色/功能/应用的关联  前端控制器
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/authUserRole")
@Api(value = "authUserRole", tags = "新权限：用户与角色或功能或应用的关联相关接口")
public class AuthUserRoleController {

    @Autowired
    private AuthUserRoleService authUserRoleService;

    @ApiOperation(value = "给用户授权用户关联角色或功能或应用")
    @PostMapping("/userRelationRole")
    public ResponseResult userRelationRole( @RequestBody @Valid AssignPermissionRQ rq) {

        if (ObjectUtils.isEmpty(rq)||ObjectUtils.isEmpty(rq.getUserId())||ObjectUtils.isEmpty(rq.getEnterpriseId())) {
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        authUserRoleService.assignPermissions(rq.getUserId(),rq.getEnterpriseId(),rq.getUserRoleList());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation(value = "查询用户在企业下的角色信息")
    @GetMapping("/getUserInCompanyRoleList")
    public ResponseResult<List<UserRelationRoleRQ>> getUserInCompanyRoleList(@RequestParam Integer userId,@RequestParam Integer enterpriseId){
        if (ObjectUtils.isEmpty(userId)||ObjectUtils.isEmpty(enterpriseId)) {
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<UserRelationRoleRQ> dto =   authUserRoleService.getUserInCompanyRoleList(userId,enterpriseId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }



    @ApiOperation(value = "查询用户下的所有应用树")
    @PostMapping("/getUserRoleTreeList")
    public ResponseResult<List<AuthUserRoleTreeDTO>> getUserRoleTreeList(@RequestParam() Integer userId) {
        if(ObjectUtils.isEmpty(userId)){
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        List<AuthUserRoleTreeDTO> userRoleTreeList = authUserRoleService.getUserRoleTreeList(userId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userRoleTreeList);
    }


    @ApiOperation(value = "查询用户拥有的角色ids")
    @GetMapping("/getUserRoleIds")
    public ResponseResult getUserRoleIds(@RequestParam  Integer userId) {
        if(ObjectUtils.isEmpty(userId)){
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        return authUserRoleService.getUserRoleIds(userId);
    }


    @ApiOperation(value = "查询用户下的接口url")
    @GetMapping("/getUserInterfaceUri")
    public ResponseResult<List<UserInterfaceUriDTO>> getUserInterfaceUri(@RequestParam() Integer userId, @RequestParam() String subSys) {
        if(ObjectUtils.isEmpty(userId)||ObjectUtils.isEmpty(subSys)){
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<UserInterfaceUriDTO> dto = authUserRoleService.getUserInterfaceUri(userId, subSys);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "查询所有应用功能角色树")
    @GetMapping("/getRoleRoleTree")
    public ResponseResult<List<AuthRoleRoleTreeDTO>> getRoleRoleTree(){

        List<AuthRoleRoleTreeDTO> dto = authUserRoleService.getRoleRoleTree();

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }

}


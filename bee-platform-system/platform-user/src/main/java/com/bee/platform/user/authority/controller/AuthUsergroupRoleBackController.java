package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.UserGroupBackRelationRoleDTO;
import com.bee.platform.user.authority.rq.AssignPermissionToBackUserGroupRQ;
import com.bee.platform.user.authority.rq.AssignPermissionToUsergroupRQ;
import com.bee.platform.user.authority.rq.UsergroupRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthUsergroupRoleBackService;
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
 * 用户组与角色/功能/应用的关联表 前端控制器
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authUsergroupRoleBack", tags = "新权限：后台用户组权限相关接口-后台")
@RequestMapping("/authUsergroupRoleBack")
public class AuthUsergroupRoleBackController {
    @Autowired
    private AuthUsergroupRoleBackService authUsergroupRoleBackService;

    @ApiOperation(value = "给后台用户组授权-后台用户组关联角色或功能或应用")
    @PostMapping("/userGroupBackRelationRole")
    public ResponseResult<String> backUserGroupRelationRole(@RequestBody @Valid AssignPermissionToBackUserGroupRQ rq) {

        if (ObjectUtils.isEmpty(rq) || ObjectUtils.isEmpty(rq.getUsergroupId())) {
            log.info("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        authUsergroupRoleBackService.assignPermissionsToBackUserGroup(rq.getUsergroupId(), rq.getBackUserGroupRoleList());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation(value = "查询后台用户组的角色信息")
    @GetMapping("/getUserGroupRoleBackList")
    public ResponseResult<List<UserGroupBackRelationRoleDTO>> getUserGroupRoleBackList(@RequestParam Integer userGroupId) {
        if (ObjectUtils.isEmpty(userGroupId) ) {
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<UserGroupBackRelationRoleDTO> dto = authUsergroupRoleBackService.getUserGroupRoleBackList(userGroupId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}


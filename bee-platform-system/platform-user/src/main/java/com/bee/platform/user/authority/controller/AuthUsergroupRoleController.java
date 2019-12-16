package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.rq.AssignPermissionToUsergroupRQ;
import com.bee.platform.user.authority.rq.UsergroupRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthUsergroupRoleService;
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
@Api(value = "authUsergroupRole", tags = "新权限：用户组权限相关接口")
@RequestMapping("/authUsergroupRole")
public class AuthUsergroupRoleController {
    @Autowired
    private AuthUsergroupRoleService authUsergroupRoleService;

    @ApiOperation(value = "给用户组授权-用户组关联角色或功能或应用")
    @PostMapping("/usergroupRelationRole")
    public ResponseResult<String> usergroupRelationRole(@RequestBody @Valid AssignPermissionToUsergroupRQ rq) {

        if (ObjectUtils.isEmpty(rq) || ObjectUtils.isEmpty(rq.getUsergroupId()) || ObjectUtils.isEmpty(rq.getEnterpriseId())) {
            log.info("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        authUsergroupRoleService.assignPermissions(rq.getUsergroupId(), rq.getEnterpriseId(), rq.getUsergroupRoleList());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation(value = "查询企业下的用户组的角色信息")
    @GetMapping("/getUsergroupRoleList")
    public ResponseResult<List<UsergroupRelationRoleRQ>> getUsergroupRoleList(@RequestParam Integer usergroupId, @RequestParam Integer enterpriseId) {
        if (ObjectUtils.isEmpty(usergroupId) || ObjectUtils.isEmpty(enterpriseId)) {
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<UsergroupRelationRoleRQ> dto = authUsergroupRoleService.getUsergroupRoleList(usergroupId, enterpriseId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}


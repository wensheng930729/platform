package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.BackUserRelationedRoleDTO;
import com.bee.platform.user.authority.rq.AssignPermissionRQ;
import com.bee.platform.user.authority.rq.AssignPermissionToBackUserRQ;
import com.bee.platform.user.authority.service.AuthUserRoleBackService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import org.springframework.stereotype.Controller;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 后台用户与角色/功能/应用的关联表 前端控制器
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-08-09
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authUserRoleBack", tags = "新权限：后台用户与角色或功能或应用的关联相关接口")
@RequestMapping("/authUserRoleBack")
public class AuthUserRoleBackController {

    @Autowired
    private AuthUserRoleBackService authUserRoleBackService;

    @ApiOperation(value = "给后台用户授权用户关联角色或功能或应用")
    @PostMapping("/backUserRelationRole")
    public ResponseResult backUserRelationRole(@RequestBody @Valid AssignPermissionToBackUserRQ rq) {

        if (ObjectUtils.isEmpty(rq)||ObjectUtils.isEmpty(rq.getUserId())) {
            log.info("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }

        authUserRoleBackService.assignPermissionsToBackUser(rq.getUserId(),rq.getBackUserRoleList());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation(value = "查询后台用户已关联过的角色信息")
    @GetMapping("/getUserRoleBackList")
    public ResponseResult<List<BackUserRelationedRoleDTO>> getUserRoleBackList(@RequestParam Integer userId){
        if (ObjectUtils.isEmpty(userId)) {
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<BackUserRelationedRoleDTO> dto = authUserRoleBackService.getUserRoleBackList(userId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }
}


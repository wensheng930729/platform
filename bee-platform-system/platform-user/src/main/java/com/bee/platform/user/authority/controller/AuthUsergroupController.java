package com.bee.platform.user.authority.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthUsergroupDetailDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupListDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupUsersDTO;
import com.bee.platform.user.authority.rq.AuthUsergroupAddRQ;
import com.bee.platform.user.authority.rq.AuthUsergroupUpdateRQ;
import com.bee.platform.user.authority.rq.AuthUsergroupUsersListRQ;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthUsergroupService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 用户组 前端控制器
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authUsergroup", tags = "新权限：用户组相关接口")
@RequestMapping("/authUsergroup")
public class AuthUsergroupController {

    @Autowired
    private AuthUsergroupService usergroupService;
    @Autowired
    private AuthPlatformUserService userService;

    @PostMapping("/add")
    @ApiOperation(value = "新增")
    public ResponseResult addUsergroup(@RequestHeader("sysToken") String sysToken, @RequestBody AuthUsergroupAddRQ rq) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return usergroupService.addUsergroup(userInfo, rq);
    }

    @GetMapping("/delete/{id}")
    @ApiOperation(value = "删除")
    public ResponseResult deleteUsergroup(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return usergroupService.deleteUsergroup(id);
    }

    @PostMapping("/update")
    @ApiOperation(value = "修改")
    public ResponseResult updateUsergroup(@RequestHeader("sysToken") String sysToken, @RequestBody AuthUsergroupUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return usergroupService.updateUsergroup(userInfo, rq);
    }

    @GetMapping("/list")
    @ApiOperation(value = "用户组列表")
    public ResponseResult<List<AuthUsergroupListDTO>> getUsergroupList(@RequestHeader("sysToken") String sysToken, String queryName, Page page) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return usergroupService.getUsergroupList(userInfo.getOrgId(), queryName, page);
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "详情")
    public ResponseResult<AuthUsergroupDetailDTO> getUsergroupDetail(@RequestHeader("sysToken") String sysToken, @PathVariable("id") int id) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return usergroupService.getUsergroupDetail(id);
    }

    @PostMapping("/users")
    @ApiOperation(value = "用户组下用户")
    public ResponseResult<List<AuthUsergroupUsersDTO>> getGroupUsers(@RequestHeader("sysToken") String sysToken, @RequestBody AuthUsergroupUsersListRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        rq.setEnterpriseId(userInfo.getOrgId());
        return usergroupService.getGroupUsers(rq, page);
    }

    @GetMapping("/checkDefault")
    @ApiOperation(value = "检查是否有默认用户组")
    public ResponseResult<Boolean> checkHasDefault(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return usergroupService.checkHasDefault(userInfo.getOrgId());
    }
}


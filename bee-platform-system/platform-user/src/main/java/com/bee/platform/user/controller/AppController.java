package com.bee.platform.user.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.AppDTO;
import com.bee.platform.user.service.AppService;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;


/**
 * @author chengke
 * @version 1.0.0
 * @ClassName AppController
 * @Description app相关操作
 * @Date 2019/3/18 14:41
 */
@Slf4j
@RestController
@RequestMapping("/api/apps")
@CrossOrigin(origins = "*")
@Api(value = "app", tags = "app相关接口")
public class AppController {

    @Autowired
    private AppService appService;

    /*@Autowired
    private UsersService usersService;*/

    @Autowired
    private AuthPlatformUserService usersService;


    @ApiOperation("根据企业id查询应用列表")
    @GetMapping("/org")
    public ResponseResult<List<AppDTO>> getAppsListByOrgId(HttpServletRequest request) {
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appService.getAppsListByOrgId(userInfo));
    }

    @ApiOperation("根据用户信息获取app列表")
    @PostMapping("/backend")
    public ResponseResult<List<AppDTO>> getAppsList(HttpServletRequest request, @RequestParam int userId) {
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(userId)) {
            log.error("参数不全");
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        /*if (RoleType.SUPER.getDesc().equals(userInfo.getRoleName()) ||
                RoleType.ADMIN.getDesc().equals(userInfo.getRoleName())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appService.getAppsList(userInfo, userId));

        }*/

        if (usersService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.ENTERPRISE_ADMIN.getCode()) ||
                usersService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.SUPER_ADMIN.getCode())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appService.getAppsList(userInfo, userId));

        }
        log.error("没有权限");
        return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
    }

    @ApiOperation("根据用户信息获取用户应用列表")
    @GetMapping("/user")
    public ResponseResult<List<AppDTO>> getAppsListByUser(HttpServletRequest request) {

        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appService.getAppListByUser(userInfo));
    }

    @ApiOperation("用户开通应用")
    @PostMapping("/add")
    public ResponseResult addAppToUser(HttpServletRequest request,
                                       @RequestParam int[] userIds,
                                       @RequestParam int[] appIds) {

        if (ArrayUtils.isEmpty(userIds) || ArrayUtils.isEmpty(appIds)) {
            log.error("请求参数为空");
            throw new BusinessException(ResCodeEnum.USER_ID_APP_ID_EMPTY, ExceptionMessageEnum.USER_ID_APP_ID_IS_EMPTY);
        }
        // 获取当前用户信息
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        /*if (!RoleType.SUPER.getDesc().equals(userInfo.getRoleName()) &&
                !RoleType.ADMIN.getDesc().equals(userInfo.getRoleName())) {
            log.error("当前用户没有权限");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }*/
        if (!usersService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.ENTERPRISE_ADMIN.getCode()) ||
                !usersService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.SUPER_ADMIN.getCode())) {
            log.error("当前用户没有权限");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);

        }

        return appService.addAppToUser(userInfo, userIds, appIds);


    }

    @ApiOperation("用户关闭应用")
    @PostMapping("/remove")
    public ResponseResult removeAppFromUser(HttpServletRequest request,
                                                         @RequestParam int[] userIds,
                                                         @RequestParam int[] appIds) {

        if (ArrayUtils.isEmpty(userIds) || ArrayUtils.isEmpty(appIds)) {
            log.error("请求参数为空");
            throw new BusinessException(ResCodeEnum.USER_ID_APP_ID_EMPTY, ExceptionMessageEnum.USER_ID_APP_ID_IS_EMPTY);
        }
        // 获取当前用户信息
        /*UserInfo userInfo = usersService.getUserInfo(request.getHeader("sysToken"));*/
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        /*if (!RoleType.SUPER.getDesc().equals(userInfo.getRoleName()) &&
                !RoleType.ADMIN.getDesc().equals(userInfo.getRoleName())) {
            log.info("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);
        }*/
        if (!usersService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.ENTERPRISE_ADMIN.getCode()) ||
                !usersService.isManager(userInfo.getId(),userInfo.getOrgId(),EnumRoleType.SUPER_ADMIN.getCode())) {
            log.info("当前用户没有权限");
            throw new BusinessException(ResCodeEnum.NO_AUTHORITY,ExceptionMessageEnum.NO_AUTHORITY);

        }

        return appService.removeAppFromUser(userInfo, userIds, appIds);
    }

    @ApiOperation("查询全部应用列表")
    @GetMapping("/appList")
    public ResponseResult<List<AppDTO>> getAllAppsList() {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appService.getAllAppsList());
    }

}

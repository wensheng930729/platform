package com.bee.platform.user.controller;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.AppDetailDTO;
import com.bee.platform.user.dto.AppListDTO;
import com.bee.platform.user.dto.EnterprisesAppsDTO;
import com.bee.platform.user.rq.AppDetailRQ;
import com.bee.platform.user.rq.EnterprisesAppsAuditRQ;
import com.bee.platform.user.rq.EnterprisesAppsRQ;
import com.bee.platform.user.service.EnterprisesAppsService;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author xu.zheng123
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "企业产品开通相关接口", tags = "企业产品开通相关接口")
@RequestMapping("/enterprisesApps")
public class EnterprisesAppsController {
	
	@Autowired
	private EnterprisesAppsService enterprisesAppsService;
    /*@Autowired
    private UsersService usersService;
    @Autowired
    private ManageUserService manageUserService;*/
    @Autowired
    private AuthPlatformUserService userService;
	
    @ApiOperation(value = "获取产品角色申请列表-----后台接口")
    @PostMapping(value = "/applyList")
    public ResponseResult<List<EnterprisesAppsDTO>> getApplyList(@RequestParam(value = "page", defaultValue = "0") Integer page,
                                               @RequestParam(value = "size", defaultValue = "5") Integer size,
                                               @Valid @RequestBody  EnterprisesAppsRQ rq) {
    	Pagination pagination = PageUtils.transFromPage(new Page(size, page));
        return enterprisesAppsService.getApplyList(rq, pagination);
    }
    
    @ApiOperation(value = "审核产品角色申请-------后台接口")
    @PostMapping(value = "/audit")
    public ResponseResult audit(@Valid @RequestBody  EnterprisesAppsAuditRQ rq, HttpServletRequest request) {
    	/*ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);
        if (ObjectUtils.isEmpty(managerInfo)) {
            log.error("获取管理员信息失败，类:{0} 方法:{1}", "UserController","getUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterprisesAppsService.audit(rq,userInfo);
    }

    @ApiOperation(value = "产品申请开通")
    @PostMapping(value = "/openApp")
    public ResponseResult<ResCodeEnum> openApp(@RequestBody AppDetailRQ appDetailRQ, HttpServletRequest request) {
        /*String sysToken=request.getHeader(SYS_TOKEN);
        UserInfo userInfo=usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if(CollectionUtils.isEmpty(appDetailRQ.getAppRoleList())){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return enterprisesAppsService.openApp(appDetailRQ, userInfo);
    }

    @ApiOperation(value = "获取产品信息-新开通")
    @GetMapping(value = "/getInfoForOpenApp")
    public ResponseResult<AppDetailDTO> getInfoForOpenApp(HttpServletRequest request, @RequestParam String id) {
        /*String sysToken=request.getHeader(SYS_TOKEN);
        UserInfo userInfo=usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if(ObjectUtils.isEmpty(id)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return enterprisesAppsService.getInfoForOpenApp(id, userInfo);
    }

    @ApiOperation(value = "获取未开通产品列表")
    @GetMapping(value = "/listNotOpenedApp")
    public ResponseResult<List<AppListDTO>> listNotOpenedApp(HttpServletRequest request) {
        /*String sysToken=request.getHeader(SYS_TOKEN);
        UserInfo userInfo=usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterprisesAppsService.listNotOpenedApp(userInfo);
    }

    @ApiOperation(value = "获取已开通产品列表-未开通角色")
    @GetMapping(value = "/listNotOpenedRoles")
    public ResponseResult<List<AppDetailDTO>> listNotOpenedRoles(HttpServletRequest request) {
        /*String sysToken=request.getHeader(SYS_TOKEN);
        UserInfo userInfo=usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterprisesAppsService.listOpenedAppNoRoles(userInfo);
    }

    @ApiOperation(value = "获取已开通产品列表-已开通角色")
    @GetMapping(value = "/listOpenedRoles")
    public ResponseResult<List<AppDetailDTO>> listOpenedRoles(HttpServletRequest request) {
        /*String sysToken=request.getHeader(SYS_TOKEN);
        UserInfo userInfo=usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }*/
        AuthPlatformUserInfo userInfo = userService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return enterprisesAppsService.listOpenedAppWithRoles(userInfo);
    }



}


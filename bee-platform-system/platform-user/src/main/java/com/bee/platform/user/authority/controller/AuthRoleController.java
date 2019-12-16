package com.bee.platform.user.authority.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.rq.AuthRoleAddRQ;
import com.bee.platform.user.authority.rq.AuthRoleRQ;
import com.bee.platform.user.authority.rq.RoleQueryRQ;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthRoleService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 角色表 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "authRole", tags = "新权限：角色相关接口")
@RequestMapping("/authRole")
public class AuthRoleController {

    @Autowired
    private AuthRoleService roleService;

    @Autowired
    private AuthPlatformUserService userService;

    @PostMapping("/listRolesBack")
    @ApiOperation(value = "获取应用角色列表-后台")
    public ResponseResult<List<AuthRoleDTO>> listAppRolesBack(HttpServletRequest request, @RequestBody AuthRoleRQ roleRQ , Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        List<AuthRoleDTO> list = roleService.listRolesBack(pagination,roleRQ);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @PostMapping("/addRolesBack")
    @ApiOperation(value = "添加/修改角色-后台")
    public ResponseResult<ResCodeEnum> addAppRolesBack(HttpServletRequest request, @RequestBody AuthRoleAddRQ rq){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return roleService.addRolesBack(userInfo, rq);
    }

    @PostMapping("/listRolesBackNoPage")
    @ApiOperation(value = "获取功能角色列表-不分页")
    public ResponseResult<List<AuthRoleDTO>> listRolesBackNoPage(HttpServletRequest request, @RequestBody RoleQueryRQ rq){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,roleService.listRolesBackNoPage(rq));
    }

    @GetMapping("/listSubRolesBackNoPage")
    @ApiOperation(value = "获取下一级角色列表-不分页")
    public ResponseResult<List<AuthRoleDTO>> listSubRolesBackNoPage(HttpServletRequest request, @RequestParam String id){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,roleService.listSubRolesBackNoPage(id));
    }

    @GetMapping("/deleteRole")
    @ApiOperation(value = "删除角色")
    public ResponseResult<ResCodeEnum> deleteRole(HttpServletRequest request, @RequestParam String id){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return roleService.deleteRole(userInfo,id);
    }

    @GetMapping("/getRoleDetail")
    @ApiOperation(value = "获取角色详情")
    public ResponseResult<AuthRoleDetailDTO> getRoleDetail(HttpServletRequest request, @RequestParam String id){
        if (StringUtils.isEmpty(id)) {
            log.info("参数错误");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return roleService.getRoleDetail(id);
    }

    @GetMapping("/listRoleSystemCode")
    @ApiOperation(value = "获取角色码表")
    public ResponseResult<List<SystemCodeDTO>> listRoleSystemCode(HttpServletRequest request){
        return roleService.listRoleSystemCode();
    }

    @GetMapping("/listSubSystemCode")
    @ApiOperation(value = "获取子系统码表")
    public ResponseResult<List<SystemCodeDTO>> listSubSystemCode(HttpServletRequest request){
        return roleService.listSubSystemCode();
    }


    @GetMapping("/getAppRole")
    @ApiOperation(value = "获取用户应用角色（中台面板使用）")
    public ResponseResult<List<AuthAppRoleDTO>> getAppRole(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return roleService.getAppRole(userInfo);
    }

    @GetMapping("/getNotOpenAppRole")
    @ApiOperation(value = "获取用户未开通应用角色（中台面板使用）")
    public ResponseResult<List<AuthAppRoleDTO>> getNotOpenAppRole(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return roleService.getNotOpenAppRole(userInfo);
    }

    @ApiOperation(value = "查询后台应用功能角色树")
    @GetMapping("/getBackRoleTree")
    public ResponseResult<List<AuthBackRoleTreeDTO>> getBackRoleTree(){

        List<AuthBackRoleTreeDTO> dto = roleService.getBackRoleTree();

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }


}


package com.bee.platform.user.controller;


import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.MRoleGroupDTO;
import com.bee.platform.user.dto.MRoleListDTO;
import com.bee.platform.user.service.MRolesService;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.vo.ManagerRoleVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @notes 管理员角色表 前端控制器
 * @Author junyang.li
 * @Date 16:16 2019/4/29
 **/
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/manager")
@Api(tags = "后台管理——权限配置")
public class MRolesController {

    @Autowired
    private MRolesService rolesService;

    @Autowired
    private ManageUserService manageUserService;

    @Autowired
    private AuthPlatformUserService userService;

    @GetMapping("/permission/group")
    @ApiOperation("权限配置权限列表查询")
    public ResponseResult<List<MRoleListDTO>> getPermissionGroup(){
        return rolesService.getPermissionGroup();
    }


    @PostMapping("/role/detail")
    @ApiOperation("根据权限id获得权限详细，不传参数则查所有的基础角色")
    public ResponseResult<List<MRoleGroupDTO>> getRoleDetail(@RequestHeader("sysToken") String sysToken, Integer roleId){
        return rolesService.getRoleDetail(roleId);
    }

    @PostMapping("/del/role")
    @ApiOperation("删除角色组")
    public ResponseResult<ResCodeEnum> deleteRole(@RequestHeader("sysToken") String sysToken,Integer roleId){
        if(roleId==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_ID_NOT_NULL);
        }
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return rolesService.deleteRole(managerInfo,roleId);
    }

    @PostMapping("/edit/role")
    @ApiOperation("编辑角色组")
    public ResponseResult<ResCodeEnum> editRole(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid ManagerRoleVO vo){
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return rolesService.editRole(managerInfo,vo);
    }





    @PostMapping("/edit/permission")
    @ApiOperation(value ="编辑基础角色的分组类别",hidden = true)
    public ResponseResult<ResCodeEnum> updatePermission(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid ManagerRoleVO vo){
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return rolesService.updatePermission(managerInfo,vo);
    }

    @PostMapping("/remove/permission")
    @ApiOperation(value ="删除基础角色分组",hidden = true)
    public ResponseResult<ResCodeEnum> removePermission(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid ManagerRoleVO vo){
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return rolesService.editRole(managerInfo,vo);
    }


    @PostMapping("/create/basic/role")
    @ApiOperation(value ="新增基础角色分组",hidden = true)
    public ResponseResult<ResCodeEnum> createBasicRole(@RequestHeader("sysToken") String sysToken,Integer roleId){
        if(roleId==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_ID_NOT_NULL);
        }
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return rolesService.deleteRole(managerInfo,roleId);
    }

    @PostMapping("/update/basic/role")
    @ApiOperation(value ="编辑角色组",hidden = true)
    public ResponseResult<ResCodeEnum> updateBasicRole(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid ManagerRoleVO vo){
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return rolesService.editRole(managerInfo,vo);
    }

    @PostMapping("/remove/basic/role")
    @ApiOperation(value ="编辑角色组",hidden = true)
    public ResponseResult<ResCodeEnum> removeBasicRole(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid ManagerRoleVO vo){
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return rolesService.editRole(managerInfo,vo);
    }
}


package com.bee.platform.user.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.MResourceDTO;
import com.bee.platform.user.service.MResourceService;
import com.bee.platform.user.service.ManageUserService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-13
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/resource")
@Api(tags = "后台管理——菜单资源相关接口")
public class MResourceController {

    @Autowired
    private MResourceService resourceService;

    @Autowired
    private ManageUserService manageUserService;

    @Autowired
    private AuthPlatformUserService userService;

    @GetMapping("/menu/tree")
    @ApiOperation(value = "查询所有的菜单树")
    public ResponseResult<List<MResourceDTO>> menuTree(){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,resourceService.menuTree());
    }

    @GetMapping("/menu/selfTree")
    @ApiOperation(value = "查询当前用户可访问的的菜单树")
    public ResponseResult<List<MResourceDTO>> menuSelfTree(@RequestHeader("sysToken") String sysToken){
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,resourceService.menuSelfTree(managerInfo.getRoleInfo()));
    }

    @GetMapping("/self/button")
    @ApiOperation(value = "查询当前用户可访问的的按钮")
    public ResponseResult<Map<String,Object>> getSelfButton(@RequestHeader("sysToken") String sysToken){
        //TODO 该接口由于新版本权限未做到按钮级别的权限校验，暂时写死
        AuthPlatformUserInfo userInfo=userService.getSelfInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,resourceService.getSelfButton());
    }
}


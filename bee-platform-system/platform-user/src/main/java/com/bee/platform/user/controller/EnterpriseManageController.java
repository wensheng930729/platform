package com.bee.platform.user.controller;

import com.bee.platform.business.dto.AllCheckTypeDTO;
import com.bee.platform.business.dto.EnterprisesWithAppDTO;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.service.EnterpriseManageService;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-05-07
 */
@RestController
@CrossOrigin(origins = "*")
@Api(value = "enterpriseManage", tags = "企业管理相关接口")
@RequestMapping("/api/enterpriseManage")
public class EnterpriseManageController {

    @Autowired
    private EnterpriseManageService enterpriseManageService;
    /*@Autowired
    private UsersService usersService;*/
    @Autowired
    private AuthPlatformUserService authPlatformUserService;
    /*@Autowired
    private ManageUserService manageUserService;*/

    @Autowired
    private AuthPlatformUserService usersService;

    @ApiOperation(value = "获取审核结果类型及对应企业数")
    @GetMapping(value = "/getTypeWithCount")
    public ResponseResult<AllCheckTypeDTO> getTypeWithCount(HttpServletRequest request) {
        /*ManagerInfo managerInfo = manageUserService.getManagerInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterpriseManageService.getTypeWithCount(userInfo);
    }

    @ApiOperation(value = "企业管理条件查询列表")
    @GetMapping(value = "/getEnterpriseList")
    public ResponseResult<List<EnterprisesWithAppDTO>> getEnterpriseList(@RequestHeader("sysToken") String sysToken, Integer typeId,String name,Page page) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterpriseManageService.getEnterpriseList(userInfo,typeId,name,page);
    }

    @PostMapping("/resetPassword")
    @ApiOperation("企业管理员为企业用户重置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "用户id",name = "userId",required = true),
            @ApiImplicitParam(value = "重置密码的方式，0为手机号重置，1为邮箱重置",name = "type",defaultValue = "0,1",required = true)
    })
    public ResponseResult<ResCodeEnum> resetPassword(@RequestHeader("sysToken") String sysToken,
                                                           @RequestParam Integer userId,@RequestParam Integer type){
        /*UserInfo userInfo = usersService.getUserInfo(sysToken);*/
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(sysToken);
        if(userId==null|| Status.getStatus(type)==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return enterpriseManageService.resetPassword(userInfo,userId,type);
    }
    
    @ApiOperation(value = "企业申请取消认证")
    @GetMapping(value = "/cancelApply")
    public ResponseResult cancelApply(HttpServletRequest request,Integer checkId) {
        /*UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));*/
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)){
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return enterpriseManageService.cancelApply(checkId,userInfo);
    }

}


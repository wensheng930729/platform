package com.bee.platform.user.controller;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.user.dto.ManagerUsersDTO;
import com.bee.platform.user.enums.AccountType;
import com.bee.platform.user.service.ManageUserService;
import com.bee.platform.user.vo.PlatformManagerEditVO;
import com.bee.platform.user.vo.PlatformManagerVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @description:  管理后台的用户管理相关接口
 * @author: junyang.li
 * @create: 2019-04-25 13:34
 **/
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/manager")
@Api(value = "后台管理——用户相关接口", tags = "后台管理——用户相关接口")
public class ManageUserController {

    @Autowired
    private ManageUserService manageUserService;


    @GetMapping("/getSelfInfo")
    @ApiOperation("获得后台用户的登录信息")
    public ResponseResult<ManagerInfo>  getManagerInfo(@RequestHeader("sysToken")String sysToken){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,manageUserService.getManagerInfo(sysToken));
    }

    @GetMapping("/detail")
    @ApiOperation("后台用户根据id获得用户信息")
    @ApiImplicitParam(value = "后台用户id",name = "managerId")
    public ResponseResult<ManagerInfo>  getManagerDetail(@RequestParam Integer managerId){
        if(managerId==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,manageUserService.getManagerDetail(managerId));
    }

    @PostMapping("/editUser")
    @ApiOperation("管理后台编辑用户")
    public ResponseResult<ResCodeEnum> editUser(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid PlatformManagerVO vo){
        if(vo.getRoleId()==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_ID_NOT_NULL);
        }
        if(!StringUtils.isEmpty(vo.getEmail()) && !Validator.isEmail(vo.getEmail())){
            return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
        }
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return manageUserService.editUser(managerInfo,vo);
    }

    @GetMapping("/phone/checkCode")
    @ApiOperation("管理后台个人中心修改手机号，请求手机验证码")
    public ResponseResult<ResCodeEnum> getPhoneCheckCode(@RequestHeader("sysToken") String sysToken,@RequestParam String phone){
        if(!Validator.isMobile(phone)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        if(managerInfo.getUsername().equals(phone)){
            return ResponseResult.buildResponseResult(ResCodeEnum.OLD_PHONE_SAME_NEW);
        }
        return manageUserService.getPhoneCheckCode(managerInfo,phone);
    }

    @GetMapping("/email/checkCode")
    @ApiOperation("管理后台个人中心修改邮箱，邮箱验证码")
    public ResponseResult<ResCodeEnum> getEmailCheckCode(@RequestHeader("sysToken") String sysToken,@RequestParam String email){
        if(!Validator.isEmail(email)){
            return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
        }
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        if(managerInfo.getEmail().equals(email)){
            return ResponseResult.buildResponseResult(ResCodeEnum.OLD_EMAIL_SAME_NEW);
        }
        return manageUserService.getEmailCheckCode(managerInfo,email);
    }

    @PostMapping("/edit")
    @ApiOperation("个人中心修改个人资料")
    public ResponseResult<ResCodeEnum> updateManager(@RequestHeader("sysToken")String sysToken,@RequestBody @Valid PlatformManagerEditVO editVO){
        if(!StringUtils.isEmpty(editVO.getEmail()) && !Validator.isEmail(editVO.getEmail())){
            return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
        }
        //获取用户信息
        ManagerInfo managerInfo=manageUserService.getManagerInfo(sysToken);
        return manageUserService.updateManager(managerInfo,editVO);
    }

    @GetMapping("/getUserList")
    @ApiOperation("权限-用户管理 获取用户列表")
    public ResponseResult<List<ManagerUsersDTO>> getUserList(Page page){
        Pagination pagination= PageUtils.transFromPage(page);
        return manageUserService.getUserList(pagination);
    }

    @PostMapping("/editAccountStatus")
    @ApiOperation("权限-用户管理 用户列表账号启禁用")
    @ApiImplicitParam(value = "用户id",name = "managerId",required = true)
    public ResponseResult<ResCodeEnum> editAccountStatus(@RequestHeader("sysToken") String sysToken,
                                                         @RequestParam Integer managerId){
        ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);
        if(managerId==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        //不允许禁用自己
        if(managerId.equals(managerInfo.getManagerId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PROHIBIT_YOURSELF);
        }
        return manageUserService.editAccountStatus(managerInfo,managerId);
    }

    @PostMapping("/resetMemberPassword")
    @ApiOperation("权限-管理员为用户重置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "用户id",name = "managerId",required = true),
            @ApiImplicitParam(value = "重置密码的方式，0为手机号重置，1为邮箱重置",name = "type",defaultValue = "0,1",required = true)
    })
    public ResponseResult<ResCodeEnum> resetMemberPassword(@RequestHeader("sysToken") String sysToken,
                                                         @RequestParam Integer managerId,@RequestParam Integer type){
        ManagerInfo managerInfo = manageUserService.getManagerInfo(sysToken);
        if(managerId==null|| AccountType.getType(type)==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return manageUserService.resetMemberPassword(managerInfo,managerId,type);
    }
}

package com.bee.platform.user.authority.controller;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.UserType;
import com.bee.platform.common.utils.CommonUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.rq.AuthUserRQ;
import com.bee.platform.user.authority.service.AuthUserLoginService;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

/**
 * @Description 登录
 * @Date 2019/5/24 10:17
 * @Author xin.huang
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/authPlatformUser")
@Api(value = "新权限：用户登录接口", tags = "新权限：用户登录接口")
public class AuthUserLoginController {

    @Autowired
    private AuthUserLoginService authUserLoginService;

    @ApiOperation(value = "账号密码登录", notes = "用户登录")
    @RequestMapping(value = "/login", method = RequestMethod.POST)
    public ResponseResult<AuthPlatformUserInfo> login(@RequestBody() @Valid AuthUserRQ authUserRQ){
        if(StringUtils.isEmpty(authUserRQ.getUsername())||StringUtils.isEmpty(authUserRQ.getPassword())){
            return ResponseResult.buildResponseResult(ResCodeEnum.USERNAME_OR_PASSWORD_ERROR);
        }
        ArrayList<Integer> userTypeList = Lists.newArrayList(UserType.MIDDLE_USER.getKey(), UserType.BACKGROUND_USER.getKey(), UserType.ORDINARY_USER.getKey(), UserType.MIDDLE_BACKGROUND_USER.getKey());
        Preconditions.checkArgument(userTypeList.contains(authUserRQ.getUserType()), "不支持的用户类型");
        return authUserLoginService.login(authUserRQ);
    }

    @ApiOperation(value = "验证码登录", notes = "用户登录")
    @RequestMapping(value = "/verification/code/login", method = RequestMethod.POST)
    public ResponseResult<AuthPlatformUserInfo> codeLogin(@RequestParam String username, @RequestParam String code){
        log.info("本次登录的账号是{}，验证码是{}",username,code);
        if(StringUtils.isEmpty(username)||StringUtils.isEmpty(code)){
            return ResponseResult.buildResponseResult(ResCodeEnum.USERNAME_OR_PASSWORD_ERROR);
        }
        return authUserLoginService.codeLogin(username,code);
    }

    @ApiOperation(value = "注销登录")
    @RequestMapping(value = "/logout", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> logout(HttpServletRequest request) {
        String sysToken= CommonUtils.getParam(request, ConstantsUtil.SYS_TOKEN);
        if(StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        try {
            return authUserLoginService.logout(sysToken);
        }catch (Exception e){
            log.error("注销登录系统异常，异常信息是：{}",e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}

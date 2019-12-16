package com.bee.platform.user.controller;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.CommonUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.service.LoginService;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;

/**
 * @description: 登录
 * @author: junyang.li
 * @create: 2019-03-04 16:41
 **/
@Slf4j
@RestController
@CrossOrigin(origins = "*")
public class LoginController {

    @Autowired
    private LoginService loginService;

    @ApiOperation(value = "账号密码登录", notes = "用户登录")
    @RequestMapping(value = "/user/login", method = RequestMethod.POST)
    public ResponseResult<String> login(@RequestParam String username, @RequestParam String password){
        if(StringUtils.isEmpty(username)||StringUtils.isEmpty(password)){
            return ResponseResult.buildResponseResult(ResCodeEnum.USERNAME_OR_PASSWORD_ERROR);
        }
        return loginService.login(username,password);
    }

    @ApiOperation(value = "验证码登录", notes = "用户登录")
    @RequestMapping(value = "/verification/code/login", method = RequestMethod.POST)
    public ResponseResult<String> codeLogin(@RequestParam String username, @RequestParam String code){
        log.info("本次登录的账号是{}，验证码是{}",username,code);
        if(StringUtils.isEmpty(username)||StringUtils.isEmpty(code)){
            return ResponseResult.buildResponseResult(ResCodeEnum.USERNAME_OR_PASSWORD_ERROR);
        }
        return loginService.codeLogin(username,code);
    }

    @ApiOperation(value = "注销登录")
    @RequestMapping(value = "/logout", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> logout(HttpServletRequest request) {
        String sysToken= CommonUtils.getParam(request, ConstantsUtil.SYS_TOKEN);
        if(StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        try {
            return loginService.logout(sysToken);
        }catch (Exception e){
            log.error("注销登录系统异常，异常信息是：{}",e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}

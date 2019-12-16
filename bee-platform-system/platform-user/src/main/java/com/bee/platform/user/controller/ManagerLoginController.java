package com.bee.platform.user.controller;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.GeetestDTO;
import com.bee.platform.user.geetest.check.StartCaptcha;
import com.bee.platform.user.service.ManagerLoginService;
import com.bee.platform.user.vo.ManagerLoginVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-04-30 10:27
 **/
@RestController
@Slf4j
@CrossOrigin(origins = "*")
@Api(tags = "后台管理——登录接口")
@RequestMapping("/manager")
public class ManagerLoginController {

    @Autowired
    private StartCaptcha startCaptcha;

    @Autowired
    private ManagerLoginService managerLoginService;


    @ApiOperation(value = "极验第一次请求校验的接口")
    @GetMapping("/verification/code")
    public String getVerificationCode(){
        ResponseResult<GeetestDTO> result= startCaptcha.initialize();
        return JSONObject.toJSONString(result.getObject());
    }


    @ApiOperation(value = "管理员登陆")
    @RequestMapping(value = "/login", method = RequestMethod.POST,consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
    public ResponseResult<String> login(@Valid ManagerLoginVO vo) {
        return managerLoginService.login(vo);
    }


    @ApiOperation(value = "注销登录")
    @PostMapping(value = "/logout")
    public ResponseResult<ResCodeEnum> logout(@RequestHeader("sysToken") String sysToken) {
        if(StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        return managerLoginService.logout(sysToken);
    }
}

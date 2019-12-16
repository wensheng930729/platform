package com.bee.platform.user.service;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-04 16:54
 **/
public interface LoginService {
    /**
     * @notes 登录
     * @Author junyang.li
     * @Date 16:56 2019/3/4
     **/
    ResponseResult<String>  login(String username,String password);

    /**
     * @notes 短信验证码登录
     * @Author junyang.li
     * @Date 10:41 2019/3/14
     **/
    ResponseResult<String> codeLogin(String username ,String code);
    /**
     * @notes 注销登录
     * @Author junyang.li
     * @Date 9:50 2019/3/5
     **/
    ResponseResult<ResCodeEnum> logout(String sysToken);
}

package com.bee.platform.user.authority.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.rq.AuthUserRQ;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-04 16:54
 **/
public interface AuthUserLoginService {
    /**
     * @notes 登录
     * @Author junyang.li
     * @Date 16:56 2019/3/4
     **/
    ResponseResult<AuthPlatformUserInfo> login(AuthUserRQ authUserRQ);

    /**
     * @notes 短信验证码登录
     * @Author junyang.li
     * @Date 10:41 2019/3/14
     **/
    ResponseResult<AuthPlatformUserInfo> codeLogin(String username, String code);
    /**
     * @notes 注销登录
     * @Author junyang.li
     * @Date 9:50 2019/3/5
     **/
    ResponseResult<ResCodeEnum> logout(String sysToken);
}

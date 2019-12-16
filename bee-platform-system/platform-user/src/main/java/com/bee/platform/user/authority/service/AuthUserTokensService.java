package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.entity.UserToken;

/**
 * @notes 用户登录凭证
 * @Author junyang.li
 * @Date 17:12 2019/3/4
 **/
public interface AuthUserTokensService extends IService<UserToken> {
    /**
     * @notes 密码校验成功，获取登录凭证
     * @Author junyang.li
     * @Date 17:13 2019/3/4
     **/
    ResponseResult<AuthPlatformUserInfo> getSysToken(AuthPlatformUser user);
    /**
     * @notes 通过username修改UserToken中的字段
     * @Author junyang.li
     * @Date 10:02 2019/3/5
     **/
    void updateByParam(UserToken userToken);

    /**
     * @notes  创建token
     * @Author junyang.li
     * @Date 15:40 2019/3/19
     **/
    String createSysToken();
    /**
     * @notes 获取sysToken过期时间秒数
     * @Author junyang.li
     * @Date 15:40 2019/3/19
     **/
    int getExpireSeconds();
}

package com.bee.platform.user.service;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.vo.ManagerLoginVO;

/**
 * @description: 工业云后台用户登录
 * @author: junyang.li
 * @create: 2019-04-30 10:29
 **/
public interface ManagerLoginService {

    /**
     * @notes 用户登录
     * @Author qhwang
     * @Date 2019-3-14 16:21:07
     **/
    ResponseResult<String> login(ManagerLoginVO vo);
    /**
     * @notes: 用户注销登录
     * @Author: junyang.li
     * @Date: 11:39 2019/5/21
     * @param sysToken : 用户登录凭证
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> logout(String sysToken);
}

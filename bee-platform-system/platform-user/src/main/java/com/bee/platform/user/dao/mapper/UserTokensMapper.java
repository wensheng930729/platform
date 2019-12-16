package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.UserToken;
import com.baomidou.mybatisplus.mapper.BaseMapper;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-04
 */
public interface UserTokensMapper extends BaseMapper<UserToken> {

    void updateByParam(UserToken userToken);
    /**
     * @notes 查询该用户信息是否存在
     * @Author junyang.li
     * @Date 13:09 2019/3/6
     **/
    UserToken selectBasc(String userName);
}

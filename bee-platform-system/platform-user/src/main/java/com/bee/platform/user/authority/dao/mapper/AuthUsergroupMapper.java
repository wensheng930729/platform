package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.authority.dto.AuthUsergroupUsersDTO;
import com.bee.platform.user.authority.entity.AuthUsergroup;
import com.bee.platform.user.authority.rq.AuthUsergroupUsersListRQ;

import java.util.List;

/**
 * <p>
 * 用户组 Mapper 接口
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
public interface AuthUsergroupMapper extends BaseMapper<AuthUsergroup> {

    /**
     * 查询用户组下所有用户
     * @param rq
     * @return
     */
    List<AuthUsergroupUsersDTO> getGroupUsers(Pagination pagination, AuthUsergroupUsersListRQ rq);

}

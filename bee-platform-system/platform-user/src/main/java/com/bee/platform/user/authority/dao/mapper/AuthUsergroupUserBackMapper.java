package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.authority.dto.AuthUsergroupUserListDTO;
import com.bee.platform.user.authority.entity.AuthUsergroupUserBack;

import java.util.List;

/**
 * <p>
 * 用户组和user关联表 Mapper 接口
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
public interface AuthUsergroupUserBackMapper extends BaseMapper<AuthUsergroupUserBack> {
    /**
     * 根据用户组id查询用户列表
     *
     * @param usergroupId 用户组id
     * @return 用户列表
     */
    List<AuthUsergroupUserListDTO> getUsergroupUserList(Integer usergroupId);
}

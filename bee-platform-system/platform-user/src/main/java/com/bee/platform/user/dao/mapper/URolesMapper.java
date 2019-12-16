package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.Role;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-04
 */
public interface URolesMapper extends BaseMapper<Role> {

    /**
     * 根据用户id获取角色列表
     * @param userId
     * @return
     */
    List<Role> selectRoleList(Integer userId);
}

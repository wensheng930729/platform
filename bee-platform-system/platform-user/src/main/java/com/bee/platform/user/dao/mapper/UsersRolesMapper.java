package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.UserRole;
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
public interface UsersRolesMapper extends BaseMapper<UserRole> {

    /**
     * 批量保存用户角色信息
     * @param userRoleList
     * @return
     */
    Integer insertAdminsList(List<UserRole> userRoleList);

    /**
     * 批量删除（修改）用户角色信息
     * @param userRoleList
     * @return
     */
    Integer updateAdminsList(List<UserRole> userRoleList);
    /**
     * @notes: 批量修改
     * @Author: junyang.li
     * @Date: 14:47 2019/5/23
     * @return: java.lang.Integer
     */
    Integer updateUserRole(UserRole userRoleList);
}

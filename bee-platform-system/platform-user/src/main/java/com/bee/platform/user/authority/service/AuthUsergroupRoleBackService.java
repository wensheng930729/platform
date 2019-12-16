package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.authority.dto.UserGroupBackRelationRoleDTO;
import com.bee.platform.user.authority.entity.AuthUsergroupRoleBack;
import com.bee.platform.user.authority.rq.UserGroupBackRelationRoleRQ;
import com.bee.platform.user.authority.rq.UsergroupRelationRoleRQ;

import java.util.List;

/**
 * <p>
 * 用户组与角色/功能/应用的关联表 服务类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
public interface AuthUsergroupRoleBackService extends IService<AuthUsergroupRoleBack> {
    /**
     * 给用户组授权
     *
     * @param usergroupId  用户组id
     * @param rq           权限列表
     */
    void assignPermissionsToBackUserGroup(Integer usergroupId, List<UserGroupBackRelationRoleRQ> rq);

    /**
     * 查询后台用户组角色信息
     *
     * @param userGroupId  后台用户组id
     * @return 角色列表
     */
    List<UserGroupBackRelationRoleDTO> getUserGroupRoleBackList(Integer userGroupId);
}

package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.authority.entity.AuthUsergroupRole;
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
public interface AuthUsergroupRoleService extends IService<AuthUsergroupRole> {
    /**
     * 给用户组授权
     * @param usergroupId 用户组id
     * @param enterpriseId 企业id
     * @param rq 权限列表
     */
    void assignPermissions(Integer usergroupId, Integer enterpriseId, List<UsergroupRelationRoleRQ> rq);

    /**
     * 查询企业下用户组角色信息
     * @param usergroupId 用户组id
     * @param enterpriseId 企业id
     * @return 角色列表
     */
    List<UsergroupRelationRoleRQ> getUsergroupRoleList(Integer usergroupId, Integer enterpriseId);
}

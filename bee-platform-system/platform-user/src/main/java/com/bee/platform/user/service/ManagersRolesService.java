package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.MRoleInfo;
import com.bee.platform.user.entity.ManagersRoles;

import java.util.List;
import java.util.Map;

/**
 * @notes 管理后台成员角色关联对象
 * @Author junyang.li
 * @Date 15:01 2019/4/29
 **/
public interface ManagersRolesService extends IService<ManagersRoles> {
    /**
     * @notes 通过用户id获得用户的角色
     * @Author junyang.li
     * @Date 15:01 2019/4/29
     **/
    MRoleInfo getUserRoles(Integer managerId);

    /**
     * @notes: 通过用户id查询用户角色，返回map对象
     * @Author: junyang.li
     * @Date: 11:21 2019/5/9
     * @param managerIds : 用户角色
     * @return: java.util.Map<java.lang.Integer,com.bee.platform.common.entity.MRoleInfo>
     */
    Map<Integer,MRoleInfo> getRoleByManagerIds(List<Integer> managerIds);

}

package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.authority.dto.AuthUsergroupUserListDTO;
import com.bee.platform.user.authority.entity.AuthUsergroupUserBack;

import java.util.List;

/**
 * <p>
 * 用户组和user关联表 服务类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
public interface AuthUsergroupUserBackService extends IService<AuthUsergroupUserBack> {
    /**
     * 用户组关联用户
     *
     * @param usergroupId 用户组id
     * @param userIds     用户id集合
     */
    void usergroupRalationUser(Integer usergroupId, List<Integer> userIds);

    /**
     * 根据用户组id查询用户列表
     *
     * @param usergroupId 用户组id
     * @return 用户列表
     */
    List<AuthUsergroupUserListDTO> getUsergroupUserList(Integer usergroupId);
}

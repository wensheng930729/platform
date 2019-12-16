package com.bee.platform.user.authority.service;

import com.bee.platform.user.authority.dto.BackUserRelationedRoleDTO;
import com.bee.platform.user.authority.entity.AuthUserRoleBack;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.authority.rq.BackUserRelationRoleRQ;

import java.util.List;

/**
 * <p>
 * 后台用户与角色/功能/应用的关联表 服务类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-08-09
 */
public interface AuthUserRoleBackService extends IService<AuthUserRoleBack> {

    void assignPermissionsToBackUser(Integer userId, List<BackUserRelationRoleRQ> rq);

    List<BackUserRelationedRoleDTO> getUserRoleBackList(Integer userId);
}

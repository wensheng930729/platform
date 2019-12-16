package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.authority.entity.AuthRoleResource;

import java.util.List;

/**
 * <p>
 * 资源角色表 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthRoleResourceMapper extends BaseMapper<AuthRoleResource> {

    /**
     *
     * @param id
     * @return
     */
    //List<AuthRoleResource> selectResourcesByUser(Integer id);

    /**
     *
     * @param orgId
     * @return
     */
    //List<AuthRoleResource> selectResourcesByOrg(Integer orgId);
}

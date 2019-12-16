package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.authority.dto.AuthRoleDTO;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
import com.bee.platform.user.authority.entity.AuthFunctionRole;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.authority.entity.AuthUsergroupRole;

import java.util.List;

/**
 * <p>
 * 功能关联角色的中间表 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthFunctionRoleMapper extends BaseMapper<AuthFunctionRole> {

    /**
     *
     * @param id
     * @return
     */
    List<AuthRoleDTO> listSubRolesBackNoPage(String id);

    /**
     *
     * @param list
     * @return
     */
    List<AuthFunctionRole> selectByFuncionTwo(List<AuthEnterpriseRole> list);

    /**
     *
     * @param list
     * @return
     */
    List<AuthFunctionRole> selectByFuncionTwoUser(List<AuthUserRole> list);

    /**
     *
     * @param authUsergroupRoles
     * @return
     */
    List<AuthFunctionRole> selectByUserGroup(List<AuthUsergroupRole> authUsergroupRoles);
}

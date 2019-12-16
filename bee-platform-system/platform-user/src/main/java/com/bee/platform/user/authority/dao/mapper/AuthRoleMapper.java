package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.authority.dto.AuthBackRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthRoleDTO;
import com.bee.platform.user.authority.dto.AuthRoleRoleTreeDTO;
import com.bee.platform.user.authority.entity.AuthFunctionRole;
import com.bee.platform.user.authority.entity.AuthRole;
import com.bee.platform.user.authority.entity.AuthUsergroupRole;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 角色表 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthRoleMapper extends BaseMapper<AuthRole> {

    /**
     * 条件查询角色列表
     * @param map
     * @param pagination
     * @return
     */
    List<AuthRole> selectRolesByCondition(Map<String, Object> map, Pagination pagination);

    /**
     *
     * @param id
     * @return
     */
    List<AuthRole> selectAppRoleByUser(@Param("id") Integer id, @Param("orgId") Integer orgId);

    /**
     * 获取用户下一级角色
     * @param id
     * @return
     */
    List<AuthRoleDTO> listSubRolesByUser(@Param("id") Integer id , @Param("orgId") Integer orgId);

    /**
     *
     * @param orgId
     * @return
     */
    List<AuthRole> selectAppRoleByOrg(Integer orgId);

    /**
     *
     * @param list
     * @return
     */
    List<AuthRole> selectAppRoleByGroup(List<AuthUsergroupRole> list);

    /**
     *
     * @param id
     * @param orgId
     * @return
     */
    List<AuthRole> selectUserApp(@Param("id") Integer id, @Param("orgId") Integer orgId);

    List<AuthBackRoleTreeDTO> getBackApplication();

    List<AuthBackRoleTreeDTO> getAllChildRole(@Param("list") List<String> appSubSys);
}

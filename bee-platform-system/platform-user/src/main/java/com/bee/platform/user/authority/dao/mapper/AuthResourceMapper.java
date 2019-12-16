package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.authority.entity.*;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 资源表 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthResourceMapper extends BaseMapper<AuthResource> {
    /**
     * 获取资源列表-分页
     * @param map
     * @param pagination
     * @return
     */
    List<AuthResource> selectResourcesByCondition(Map<String, Object> map, Pagination pagination);

    /**
     * 获取资源列表-用户
     * @param orgId
     * @return
     */
    List<AuthResource> listResourcesByUser(Integer orgId);

    /**
     * 获取资源列表-分用户分平台
     * params
     * sub_sys
     * @return
     */
    //List<AuthResource> selectResources(@Param("list") List<AuthRoleResource> list, @Param("subSys") String subSys);

    /**
     *
     * @param id
     * @return
     */
    //List<AuthResource> selectSubResource(Integer id);
    List<AuthResource> selectResources(List<Integer> params, String sub_sys);
    /**
     * @notes: 批量插入资源
     * @Author: junyang.li
     * @Date: 15:15 2019/5/28
     * @param list :
     * @return: void
     */
    void insertAll(List<AuthResource> list);

    /**
     *
     * @param list
     * @return
     */
    List<AuthResource> selectResourcesByOrgRoles(@Param("list") List<AuthFunctionRole> list, @Param("subSys")String subSys);

    /**
     *
     * @param list
     * @return
     */
    List<AuthResource> selectResourcesByUserRoles(@Param("list")List<Integer> list, @Param("subSys")String subSys);
}

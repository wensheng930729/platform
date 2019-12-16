package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.AppRoles;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 产品角色信息表 Mapper 接口
 * </p>
 *
 * @author qhwang
 * @since 2019-04-26
 */
public interface AppRolesMapper extends BaseMapper<AppRoles> {

    /**
     * 查询已开通产品-未开通角色
     * @param appId
     * @param orgId
     * @return
     */
    List<AppRoles> selectNotOpenedRoles(@Param("appId")Integer appId, @Param("orgId")Integer orgId);

    /**
     * 查询未开通产品-已开通角色
     * @param appId
     * @param orgId
     * @return
     */
    List<AppRoles> selectOpenedRoles(@Param("appId")Integer appId, @Param("orgId")Integer orgId);
}

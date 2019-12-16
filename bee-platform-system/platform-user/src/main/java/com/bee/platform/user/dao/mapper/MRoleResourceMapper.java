package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.MRoleResource;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 角色资源关联表 Mapper 接口
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-28
 */
public interface MRoleResourceMapper extends BaseMapper<MRoleResource> {
    /**
     * @notes: 根据角色查询对应的资源详细
     * @Author: junyang.li
     * @Date: 9:33 2019/5/14
     * @param list : 角色集合
     * @return: java.util.List<com.bee.platform.user.entity.MResource>
     */
    List<Integer> getResourceIdByRoleIds(List<Integer> list);
}

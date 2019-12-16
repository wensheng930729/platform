package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.MResource;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 资源相关
 * @author hongchuan.he123
 * @since 2019-04-28
 */
public interface MResourceMapper extends BaseMapper<MResource> {
    /**
     * @notes: 
     * @Author: junyang.li
     * @Date: 13:41 2019/5/13
     * @param resourceType : 资源类型
     * @return: java.util.List<com.bee.platform.user.entity.MResource>
     */
    List<MResource> getResourceByType(@Param("resourceType") Integer resourceType);
    /**
     * @notes: 根据菜单类型获得菜单的父id
     * @Author: junyang.li
     * @Date: 10:27 2019/5/14
     * @param resourceType : 菜单类型
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> getParentIdByType(int resourceType);

}

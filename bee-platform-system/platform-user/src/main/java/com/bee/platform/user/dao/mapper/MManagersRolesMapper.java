package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.dto.ManagerRoleCountDTO;
import com.bee.platform.user.entity.ManagersRoles;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @notes 用户角色关联对象
 * @Author junyang.li
 * @Date 16:41 2019/4/29
 **/
public interface MManagersRolesMapper extends BaseMapper<ManagersRoles> {

    /**
     * @notes 统计角色中的成员数量
     * @Author junyang.li
     * @Date 16:44 2019/4/29
     **/
    List<ManagerRoleCountDTO> countManagerByRoleIds(List<Integer> list);
    /**
     * @notes 统计角色的成员数
     * @Author junyang.li
     * @Date 11:16 2019/4/30
     **/
    Integer countManagerByRoleId( @Param("roleId") Integer roleId);

}

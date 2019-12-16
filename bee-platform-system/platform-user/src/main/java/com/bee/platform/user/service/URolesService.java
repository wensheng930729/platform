package com.bee.platform.user.service;

import com.bee.platform.user.entity.Role;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * @notes 角色
 * @Author junyang.li
 * @Date 17:04 2019/3/5
 **/
public interface URolesService extends IService<Role> {

    /**
     * @notes 获得所有的角色
     * @Author junyang.li
     * @Date 17:05 2019/3/5
     **/
    List<Role> getAllRole();

    /**
     * @notes 查询角色
     * @Author junyang.li
     * @Date 17:09 2019/3/5
     **/
    Role selectOne(Integer roleId);
}

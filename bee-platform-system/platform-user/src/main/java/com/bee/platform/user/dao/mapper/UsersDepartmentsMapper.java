package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.UsersDepartments;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * @notes 用户部门
 * @Author junyang.li
 * @Date 15:03 2019/3/6
 **/
public interface UsersDepartmentsMapper extends BaseMapper<UsersDepartments> {

    /**
     * @notes
     * @Author junyang.li
     * @Date 15:06 2019/3/6
     **/
    void deleteUserInDepart(Integer userId, List<Integer> list );

    /**
     * @Description 批量添加用户关联的部门
     * @Param list
     * @Date 2019/5/23 14:03
     * @Author xin.huang
     * @Return
     */
    int batchInsert(List<UsersDepartments> list);
}

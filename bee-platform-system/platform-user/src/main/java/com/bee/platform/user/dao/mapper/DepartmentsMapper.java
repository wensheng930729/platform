package com.bee.platform.user.dao.mapper;


import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.common.entity.DepartmentInfo;
import com.bee.platform.user.dto.EnterprisesCountDTO;
import com.bee.platform.user.dto.UserDepartmentDTO;
import com.bee.platform.user.entity.Departments;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @notes 部门
 * @Author junyang.li
 * @Date 9:19 2019/3/6
 **/
public interface DepartmentsMapper extends BaseMapper<Departments> {
    /**
     * @notes 通过用户id和企业id查询用户所在部门
     * @Author junyang.li
     * @Date 9:18 2019/3/6
     **/
    List<DepartmentInfo> selectByUserIdAndOrgId(@Param("userId") Integer userId, @Param("orgId")Integer orgId);

    /**
     * 根据用户id查询部门
     * @param userId
     * @return
     */
    List<Departments> selectDepartmentsByUserId(Integer userId);
    /**
     * @notes 通过企业id统计部门数量
     * @Author junyang.li
     * @Date 11:49 2019/3/18
     **/
    List<EnterprisesCountDTO> countDepartment(List<Integer> list);

    /**
     * @notes  用户管理查询用户的部门信息
     * @param orgId 企业id
     * @param list 用户id集合
     * @Author junyang.li
     * @Date 14:59 2019/3/20
     **/
    List<UserDepartmentDTO> ManagerSearchUser(int orgId, List<Integer> list);
    /**
     * @notes: 初始化数据时调整Departments表中的level字段
     * @Author: junyang.li
     * @Date: 15:09 2019/6/3
     * @param list :
     * @return: void
     */
    void updateLevelByIds(@Param("list") List<Departments> list);
}

package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.authority.dto.AuthRoleRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthUserRoleTreeDTO;
import com.bee.platform.user.authority.dto.UserInterfaceUriDTO;
import com.bee.platform.user.authority.entity.AuthUserRole;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 用户与角色（角色或功能的关联表）的中间表 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthUserRoleMapper extends BaseMapper<AuthUserRole> {
    /**
     * 查询用户角色
     * @param userId 用户id
     * @return 用户角色
     */
    List<AuthUserRoleTreeDTO>  getUserRole(Integer userId);
    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 14:28 2019/5/24
     * @param list :
     * @return: void
     */
    void insertAll(List<AuthUserRole>  list);

    /**
     * 获取用户企业ids
     * @param userId 用户id
     * @return 企业ids
     */
    List<Integer> getUserEnterpriseIds(Integer userId);

    /**
     * 查询用户在企业下角色ids
     * @param userId 用户id
     * @param enterpriseId  企业id
     * @return 角色ids
     */
    List<Integer> getRoleIds(@Param("userId") Integer userId, @Param("enterpriseId") Integer enterpriseId);

    /**
     * 查询用户下的接口url
     * @param userId 用户id
     * @param subSys 子系统标识
     * @return
     */
    List<UserInterfaceUriDTO> getUserInterfaceUri(@Param("userId") Integer userId,@Param("subSys") String subSys);

    /**
     * 查询功能所有功能一
     * @param list applicationIds
     * @return 所有功能一
     */
    List<AuthRoleRoleTreeDTO> getChildRole(@Param("list")List<Integer> list);

    /**
     * 根据用户和子系统标识获取角色
     * map
     * @return
     */
    List<AuthUserRole> selectAuthUserRole(Map<String,Object> map);

    /**
     * 查询用户在子系统中的角色
     * @param subSys
     * @return
     */
    List<AuthUserRole> getUserRoleIdBySubSys(@Param("orgId")Integer orgId,@Param("userId")Integer userId,@Param("subSys")String subSys);
}

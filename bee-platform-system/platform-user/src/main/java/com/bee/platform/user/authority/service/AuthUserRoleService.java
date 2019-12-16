package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthRoleRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthUserRoleTreeDTO;
import com.bee.platform.user.authority.dto.UserInterfaceUriDTO;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.authority.rq.AuthUserRoleRQ;
import com.bee.platform.user.authority.rq.UserRelationRoleRQ;

import java.util.List;

/**
 * <p>
 * 用户与角色（角色或功能的关联表）的中间表 服务类
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
public interface AuthUserRoleService extends IService<AuthUserRole> {


    void assignPermissions(Integer userId,Integer enterpriseId, List<UserRelationRoleRQ> rq);

    /**
     * 用户关联角色/功能/应用
     * @param rq 请求参数
     * @return 操作结果
     */
    void changeUserRole(AuthUserRoleRQ rq);

    /**
     * 查询用户角色树
     * @param userId 用户id
     * @return 用户角色树
     */
    List<AuthUserRoleTreeDTO> getUserRoleTreeList(Integer userId);
    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 14:28 2019/5/24
     * @param list :
     * @return: void
     */
    void insertAll(List<AuthUserRole> list);


    /**
     * 获取用户ids
     * @param userId
     * @return
     */
    ResponseResult getUserRoleIds(Integer userId);

    /**
     * 查询用户下的接口url
     * @param userId userId
     * @return 接口url列表
     */
    List<UserInterfaceUriDTO> getUserInterfaceUri(Integer userId,String platform);

    /**
     * 查询应用功能角色树
     * @return  应用功能角色树
     */
    List<AuthRoleRoleTreeDTO> getRoleRoleTree();

    List<UserRelationRoleRQ> getUserInCompanyRoleList(Integer userId, Integer enterpriseId);
}

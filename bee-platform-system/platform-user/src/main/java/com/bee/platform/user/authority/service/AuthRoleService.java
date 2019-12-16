package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.entity.AuthRole;
import com.bee.platform.user.authority.rq.AuthRoleAddRQ;
import com.bee.platform.user.authority.rq.AuthRoleRQ;
import com.bee.platform.user.authority.rq.RoleQueryRQ;

import java.util.List;

/**
 * <p>
 * 角色表 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthRoleService extends IService<AuthRole> {

    /**
     * 添加应用角色-后台
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> addRolesBack(AuthPlatformUserInfo userInfo, AuthRoleAddRQ rq);

    /**
     * 获取角色列表-后台
     * @param pagination
     * @return
     */
    List<AuthRoleDTO> listRolesBack(Pagination pagination, AuthRoleRQ roleRQ);

    /**
     * 获取角色-不分页
     * @param
     * @return
     */
    List<AuthRoleDTO> listRolesBackNoPage(RoleQueryRQ rq);

    /**
     * 删除角色
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> deleteRole(AuthPlatformUserInfo userInfo, String id);

    /**
     *
     * @return
     */
    ResponseResult<List<SystemCodeDTO>> listRoleSystemCode();

    /**
     * 获取角色详细信息
     * @param id
     * @return
     */
    ResponseResult<AuthRoleDetailDTO> getRoleDetail(String id);

    /**
     * 获取子系统详细信息
     * @return
     */
    ResponseResult<List<SystemCodeDTO>> listSubSystemCode();

    /**
     * 根据id查询下一级角色列表
     * @param id
     * @return
     */
    List<AuthRoleDTO> listSubRolesBackNoPage(String id);

    /**
     * 查询用户应用角色
     * @param userInfo
     * @return
     */
    ResponseResult<List<AuthAppRoleDTO>> getAppRole(AuthPlatformUserInfo userInfo);

    /**
     *
     * @param userInfo
     * @return
     */
    ResponseResult<List<AuthAppRoleDTO>> getNotOpenAppRole(AuthPlatformUserInfo userInfo);

    List<AuthBackRoleTreeDTO> getBackRoleTree();
}

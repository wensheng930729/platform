package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthRoleUsedDTO;
import com.bee.platform.user.authority.entity.AuthFunctionRole;
import com.bee.platform.user.authority.rq.AuthEnterpriseRoleConfigRQ;
import com.bee.platform.user.authority.rq.RoleRelationRoleRQ;
import com.bee.platform.user.dto.AuthEnterpriseAppFunDTO;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * 功能关联角色的中间表 服务类
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
public interface AuthFunctionRoleService extends IService<AuthFunctionRole> {

    /**
     * 角色<-->功能<-->应用对应关系变更方法
     *
     * @param creatorId 用户id
     * @param rq        请求参数
     * @return 操作结果
     */
    void changeFunctionRole(Integer creatorId, RoleRelationRoleRQ rq);

    /**
     * 查询当前用户所在企业的应用下的所有功能
     * @param enterpriseId
     * @return
     */
    ResponseResult<List<AuthRoleTreeDTO>> getEnterpriseFuns(Integer enterpriseId);

    /**
     * 查询所有应用下的所有功能
     *
     * @return
     */
    ResponseResult<List<AuthEnterpriseAppFunDTO>> getAllFuns();

    /**
     * 通过用户查询企业已开通哪些功能的id
     *
     * @param enterpriseId
     * @return
     */
    ResponseResult<List<AuthRoleUsedDTO>> getEnterpriseFunsByEnterpriseId(Integer enterpriseId);

    /**
     * 查询二级功能下的基础角色ids
     *
     * @param twoList
     * @return
     */
    List<AuthFunctionRole> getFunTwoFollowRole(List<Integer> twoList);

    /**
     * 保存企业权限配置的修改
     *
     * @param rq
     * @return
     */
    ResponseResult changeEnterpriseRole(AuthEnterpriseRoleConfigRQ rq, AuthPlatformUserInfo userInfo);
}

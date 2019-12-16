package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthUsergroupDetailDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupListDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupUsersDTO;
import com.bee.platform.user.authority.entity.AuthUsergroupBack;
import com.bee.platform.user.authority.rq.AuthUsergroupAddRQ;
import com.bee.platform.user.authority.rq.AuthUsergroupUpdateRQ;
import com.bee.platform.user.authority.rq.AuthUsergroupUsersListRQ;

import java.util.List;

/**
 * <p>
 * 用户组 服务类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
public interface AuthUsergroupBackService extends IService<AuthUsergroupBack> {

    /**
     * 添加用户组
     *
     * @param rq
     * @param userInfo
     * @return
     */
    public ResponseResult addUsergroup(AuthPlatformUserInfo userInfo, AuthUsergroupAddRQ rq);

    /**
     * 删除用户组
     *
     * @param id
     * @return
     */
    public ResponseResult deleteUsergroup(Integer id);

    /**
     * 更新用户组
     *
     * @param userInfo
     * @param rq
     * @return
     */
    public ResponseResult updateUsergroup(AuthPlatformUserInfo userInfo, AuthUsergroupUpdateRQ rq);

    /**
     * 查询用户组列表
     *
     * @param page
     * @param queryName
     * @param orgId
     * @return
     */
    public ResponseResult<List<AuthUsergroupListDTO>> getUsergroupList(Integer orgId, String queryName, Page page);

    /**
     * 查询用户组详情
     *
     * @param id
     * @return
     */
    public ResponseResult<AuthUsergroupDetailDTO> getUsergroupDetail(Integer id);

    /**
     * 根据企业id和用户id 查询用户所在用户组
     *
     * @param orgId
     * @param userId
     * @return
     */
    public List<AuthUsergroupBack> getByEnterpriseAndUserId(Integer orgId, Integer userId);

    /**
     * 查询用户组下用户信息
     *
     * @param rq
     * @return
     */
    public ResponseResult<List<AuthUsergroupUsersDTO>> getGroupUsers(AuthUsergroupUsersListRQ rq, Page page);

    /**
     * 查询企业是否有默认用户组
     *
     * @param orgId
     * @return
     */
    public ResponseResult<Boolean> checkHasDefault(int orgId);
}

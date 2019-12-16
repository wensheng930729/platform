package com.bee.platform.user.authority.service;

import java.util.List;
import java.util.Set;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthPlatformUserDto;
import com.bee.platform.user.authority.dto.AuthPlatformUserEnterpriseDTO;
import com.bee.platform.user.authority.dto.AuthUserEnterpriseDetailDto;
import com.bee.platform.user.authority.dto.AuthUserEnterprisesDto;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
import com.bee.platform.user.authority.rq.AuthPlatformUserRQ;
import com.bee.platform.user.authority.rq.AuthPlatformUserSelectRQ;

/**
 * <p>
 * 企业与用户中间表 服务类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthPlatformUserEnterpriseService extends IService<AuthPlatformUserEnterprise> {

    /**
     * 根据企业id查询企业下的用户
     *
     * @param enterpriseId
     * @return
     */
    List<AuthPlatformUser> getEnterpriseUser(Integer enterpriseId);

    /**
     * 查询企业管理员
     *
     * @param enterpriseId
     * @return
     */
    List<AuthPlatformUser> getEnterpriseAdmin(Integer enterpriseId);
    /**
     * 查询企业管理员的ids
     *
     * @param enterpriseId
     * @return
     */
    List<Integer> getEnterpriseAdminIds(Integer enterpriseId);
    /**
     * 查询企业普通用户
     *
     * @param enterpriseId
     * @return
     */
    List<AuthPlatformUser> getEnterpriseCommonUser(Integer enterpriseId);

    /**
     * 根据当前登录人查询企业
     *
     * @param userInfo
     * @return
     */
    ResponseResult<List<AuthUserEnterprisesDto>> getUserEnterprises(AuthPlatformUserInfo userInfo);

    /**
     * @Description 条件查询平台用户列表
     * @Param authInterfaceRQs
     * @Date 2019/5/21 9:20
     * @Author xin.huang
     * @Return
     */
    List<AuthPlatformUserDto> getList(String sysToken, AuthPlatformUserSelectRQ authPlatformUserSelectRQ, Pagination pagination);

    /**
     * @Description 添加平台用户
     * @Param authPlatformUserRQ
     * @Date 2019/5/21 9:21
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> add(String sysToken, AuthPlatformUserRQ authPlatformUserRQ);

    /**
     * @Description 修改平台用户
     * @Param id
     * @Param authPlatformUserRQ
     * @Date 2019/5/21 9:22
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> update(String sysToken, Integer id, AuthPlatformUserRQ authPlatformUserRQ);

    /**
     * @Description 批量删除平台用户
     * @Param ids
     * @Date 2019/5/21 9:23
     * @Author xin.huangss
     * @Return
     */
    ResponseResult<ResCodeEnum> batchDelete(String ids);

    /**
     * @Description 更新用户状态
     * @Param id
     * @Param status
     * @Date 2019/5/23 10:47
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> updateStatus(String sysToken, Integer userId, Integer enterpriseId, Integer status);

    /**
     * @Description 管理员为用户重置密码
     * @Param userId
     * @Param type
     * @Date 2019/5/23 14:45
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> resetMemberPassword(String sysToken, Integer userId, Integer type, Integer enterpriseId);

    /**
     * @param list :
     * @notes: 批量插入用户企业关系数据
     * @Author: junyang.li
     * @Date: 13:54 2019/5/24
     * @return: void
     */
    void insertAll(List<AuthPlatformUserEnterprise> list);

    /**
     * @Description 查询该用户的企业id
     * @Param userId
     * @Param orgIds
     * @Date 2019/5/24 14:01
     * @Author xin.huang
     * @Return
     */
    List<Integer> userInEnterprises(Integer userId, List<Integer> orgIds);

    /**
     * @Description 根据用户id和企业id查询企业信息
     * @Param userId
     * @Param enterpriseId
     * @Date 2019/5/24 14:53
     * @Author xin.huang
     * @Return
     */
    AuthPlatformUserEnterprise findByUserIdAndEnterpriseId(Integer userId, Integer enterpriseId);

    /**
     * @Description 查询用户所在企业的信息详情
     * @Param userId
     * @Param enterpriseId
     * @Date 2019/5/27 14:03
     * @Author xin.huang
     * @Return
     */
    AuthPlatformUserDto findUserEnterpriseInfo(Integer userId, Integer enterpriseId);

    /**
     * @Description 查询用户所在企业的信息和权限详情
     * @Param userId
     * @Param enterpriseId
     * @Date 2019/5/27 16:14
     * @Author xin.huang
     * @Return
     */
    AuthUserEnterpriseDetailDto findUserEnterpriseAndResourceInfo(AuthPlatformUserInfo userInfo, Integer userId);


    /**
     * 根据企业id和用户查询
     *
     * @param userId
     * @return
     */
    List<AuthPlatformUserEnterpriseDTO> qureyEnterpriseUser(int userId);

    List<AuthPlatformUserDto> getListOfEnterprise(AuthPlatformUserSelectRQ rq, Pagination pagination);

    /**
     * 获取指定部门和下级部门人员id
     * @param departmentIds
     * @param enterpriseId
     * @return
     */
    ResponseResult<Set<Integer>> findDepartmentIdAndEnterpriseId(List<Integer> departmentIds, Integer enterpriseId);
}

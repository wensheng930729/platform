package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthPlatformUserDto;
import com.bee.platform.user.authority.dto.AuthPlatformUserFeignDTO;
import com.bee.platform.user.authority.dto.AuthPlatformUserInDTO;
import com.bee.platform.user.authority.dto.AuthPlatformUserPullDownDto;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.rq.*;
import com.bee.platform.user.dto.CompanyDTO;
import com.bee.platform.user.dto.UserBasicDTO;
import com.bee.platform.user.rq.UserAuthValidateRQ;
import com.bee.platform.user.rq.UserInCompanyRQ;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthPlatformUserService extends IService<AuthPlatformUser> {


    /**
     * @param list : 用户集合
     * @notes: 批量插入用户
     * @Author: junyang.li
     * @Date: 10:51 2019/5/24
     * @return: void
     */
    void insertAllUser(List<AuthPlatformUser> list);

    AuthPlatformUser selectOne(AuthPlatformUser user);

    /**
     * @notes 注册第一步请求验证码
     * @Author xin.huang
     * @Date 10:43 2019/3/4
     **/
    ResponseResult<ResCodeEnum> getRegisterValidateCode(String userName);

    /**
     * @notes 内部系统获取用户信息
     * @Author xin.huang
     * @Date 18:39 2018/12/11
     **/
    AuthPlatformUserInfo getUserInfo(String sysToken);

    /**
     * @notes 通过token获得用户信息
     * @Author xin.huang
     * @Date 18:39 2018/12/11
     **/
    AuthPlatformUserInfo getSelfInfo(String sysToken);

    /**
     * @notes 校验验证码
     * @Author xin.huang
     * @Date 14:09 2019/3/4
     **/
    ResponseResult<ResCodeEnum> validateCode(String account, String code);

    /**
     * @notes 注册最后一部
     * @Author xin.huang
     * @Date 15:27 2019/3/4
     **/
    ResponseResult<ResCodeEnum> register(RegisterUserRQ rq);

    /**
     * @notes 用于存在的用户请求验证码
     * @Author xin.huang
     * @Date 11:00 2019/3/5
     **/
    ResponseResult<ResCodeEnum> sendMessage(String phone);

    /**
     * @notes 忘记密码（用户未登录）
     * @Author xin.huang
     * @Date 15:21 2019/3/5
     **/
    ResponseResult<ResCodeEnum> resetPassword(String password, String phone);

    /**
     * @notes 更新后修改密码
     * @Author xin.huang
     * @Date 15:49 2019/3/5
     **/
    ResponseResult<ResCodeEnum> updatePassword(AuthPlatformUserInfo userInfo, String newPassword);

    /**
     * @notes 查询该用户的企业id
     * @Author xin.huang
     * @Date 17:07 2019/1/18
     **/
    List<Integer> userInCompany(UserInCompanyRQ rq);

    /**
     * @notes 切换企业
     * @Author xin.huang
     * @Date 16:41 2019/3/5
     **/
    ResponseResult<ResCodeEnum> switchOrg(AuthPlatformUserInfo userInfo, Integer orgId);

    /**
     * @notes 将用户信息插入的缓存中
     * @Author xin.huang
     * @Date 11:08 2019/3/6
     **/
    void addToRedis(AuthPlatformUserInfo userInfo);

    /**
     * @notes 删除员工
     * @Author xin.huang
     * @Date 14:28 2019/3/6
     **/
    ResponseResult<ResCodeEnum> deleteUserForDepartment(AuthPlatformUserInfo userInfo, int id);

    /**
     * @notes 修改个人信息
     * @Author xin.huang
     * @Date 11:28 2019/3/14
     **/
    ResponseResult<ResCodeEnum> modifySelfInfo(AuthPlatformUserInfo userInfo, EditAuthPlatformUserRQ editUserRQ);

    /**
     * @notes 搜索用户
     * @Author xin.huang
     * @Date 13:50 2019/3/14
     **/
    ResponseResult<UserBasicDTO> findUserAndDepartment(AuthPlatformUserInfo userInfo, String name);

    /**
     * @notes 供应链金融根据关键词查询用户
     * @Author xin.huang
     * @Date 16:05 2019/3/14
     **/
    ResponseResult<List<AuthPlatformUserInfo>> getUserByKeyWord(AuthPlatformUserInfo userInfo, String keyWord, Pagination page);

    /**
     * @notes 通过用户id获取用户信息
     * @Author xin.huang
     * @Date 17:29 2019/3/21
     **/
    ResponseResult<AuthPlatformUserDto> getUserById(AuthPlatformUserInfo userInfo, Integer userId);

    /**
     * @notes 内部服务获取指定用户的企业信息
     * @Author xin.huang
     * @Date 16:32 2019/3/24
     **/
    ResponseResult<CompanyDTO> getUserInfoCompany(String username, String company);

    /**
     * @return
     * @Description 修改用户手机号或邮箱
     * @Author xin.huang
     * @Param sysToken
     * @Param editUserAccountRQ
     **/
    ResponseResult<ResCodeEnum> updateAccount(String sysToken, EditUserAccountRQ editUserAccountRQ);

    /**
     * @return
     * @Description 获取邮箱验证码
     * @Author xin.huang
     * @Param phone
     * @Param email
     **/
    ResponseResult<ResCodeEnum> sendEmail(String phone, String email);


    /**
     * 添加后台用户
     *
     * @param userInfo
     * @param authPlatformUserAfterRQ
     * @return
     * @throws Exception
     */
    @Transactional(rollbackFor = Exception.class)
    ResponseResult<ResCodeEnum> add(AuthPlatformUserInfo userInfo, AuthPlatformUserAfterRQ authPlatformUserAfterRQ) throws Exception;


    /**
     * 更新后台用户
     *
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> update(AuthPlatformUserInfo userInfo, AuthPlatformUserAfterUpdateRQ authPlatformUserAfterUpdateRQ);

    /**
     * 添加中台用户
     *
     * @param userInfo
     * @param authPlatformUserINRQ
     * @return
     */
    ResponseResult<ResCodeEnum> addIn(AuthPlatformUserInfo userInfo, AuthPlatformUserINRQ authPlatformUserINRQ);

    ResponseResult<AuthPlatformUserDto> getAuthPlatformUserById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 查询后台用户
     *
     * @param userInfo
     * @param authPlatformUserAfterSelectRQ
     * @return
     */
    ResponseResult<List<AuthPlatformUserDto>> query(AuthPlatformUserInfo userInfo, AuthPlatformUserAfterSelectRQ authPlatformUserAfterSelectRQ, Pagination pagination);

    /**
     * 查询后台用户
     *
     * @return
     */
    ResponseResult<List<AuthPlatformUserPullDownDto>> getBackUser();

    /**
     * 后天用户启用禁用
     *
     * @param userInfo
     * @param status
     * @return
     */
    ResponseResult<ResCodeEnum> updateById(AuthPlatformUserInfo userInfo, int status, int id);


    /**
     * 查询中台用户
     *
     * @param userInfo
     * @param authPlatformUserSelectINRQ
     * @return
     */
    List<AuthPlatformUserInDTO> queryIn(AuthPlatformUserInfo userInfo, AuthPlatformUserSelectINRQ authPlatformUserSelectINRQ, Pagination pagination);


    /**
     * 编辑中台用户
     *
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> updateIn(AuthPlatformUserInfo userInfo, AuthPlatformUserUpdateINRQ authPlatformUserUpdateINRQ);

    /**
     * 通用用户权限校验接口
     *
     * @param rq
     * @return
     */
    ResponseResult<Boolean> validateRequest(UserAuthValidateRQ rq);

    /**
     * 查看一个用户的
     *
     * @param userInfo
     * @return
     */
    ResponseResult<Object> queryInOne(AuthPlatformUserInfo userInfo, AuthPlatformUserOneInRQ authPlatformUserOneInRQ);

    /**
     * 编辑中台用户
     *
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> updateAuthPlatformUserOneIn(AuthPlatformUserInfo userInfo, AuthPlatformUserAfterUpdateRQ authPlatformUserAfterUpdateRQ);

    /**
     * 编辑中台用户返回的数据
     *
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<AuthPlatformUserDto> getAuthPlatformUserInById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 判断用户是否是企业管理员
     *
     * @param orgId
     * @param userId
     * @return
     */
    Boolean isManager(Integer orgId, Integer userId, String roleType);

    ResponseResult<AuthPlatformUserInfo> getAuthUserRoles(String username, Integer orgId, String sysType);

    ResponseResult<List<AuthPlatformUserPullDownDto>> getAllCompanyUserById(Integer orgId);

    /**
     * 查询多个用户详情
     *
     * @param ids
     * @return
     * @auth liliang
     * @date 2019-8-2
     */
    public ResponseResult<List<AuthPlatformUserFeignDTO>> getMoreUserInfo(List<Integer> ids);
}

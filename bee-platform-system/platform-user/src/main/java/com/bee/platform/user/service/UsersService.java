package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.*;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.User;
import com.bee.platform.user.rq.*;

import java.util.List;

/**
 * @notes 用户
 * @Author junyang.li
 * @Date 10:43 2019/3/4
 **/
public interface UsersService extends IService<User> {
    /**
     * @notes 注册第一步请求验证码
     * @Author junyang.li
     * @Date 10:43 2019/3/4
     **/
    ResponseResult<ResCodeEnum> getRegisterValidateCode(String userName);

    /**
     * @notes 内部系统获取用户信息
     * @Author junyang.li
     * @Date 18:39 2018/12/11
     **/
    UserInfo getUserInfo(String sysToken);

    /**
     * @notes 通过token获得用户信息
     * @Author junyang.li
     * @Date 18:39 2018/12/11
     **/
    UserInfo getSelfInfo(String sysToken);

    /**
     * @notes 校验验证码
     * @Author junyang.li
     * @Date 14:09 2019/3/4
     **/
    ResponseResult<ResCodeEnum> validateCode(String phone, String code);

    /**
     * @notes 注册最后一部
     * @Author junyang.li
     * @Date 15:27 2019/3/4
     **/
    ResponseResult<ResCodeEnum> register(RegisterRQ rq);

    /**
     * @notes 查询单个用户
     * @Author junyang.li
     * @Date 17:08 2019/3/4
     **/
    User selectOne(User user);

    /**
     * @notes 用于存在的用户请求验证码
     * @Author junyang.li
     * @Date 11:00 2019/3/5
     **/
    ResponseResult<ResCodeEnum> sendMessage(String phone);

    /**
     * @notes 忘记密码（用户未登录）
     * @Author junyang.li
     * @Date 15:21 2019/3/5
     **/
    ResponseResult<ResCodeEnum> resetPassword(String password, String phone);

    /**
     * @notes 更新后修改密码
     * @Author junyang.li
     * @Date 15:49 2019/3/5
     **/
    ResponseResult<ResCodeEnum> updatePassword(UserInfo userInfo, String newPassword);

    /**
     * @notes 查询该用户的企业id
     * @Author junyang.li
     * @Date 17:07 2019/1/18
     **/
    List<Integer> userInCompany(UserInCompanyRQ rq);

    /**
     * @notes 切换企业
     * @Author junyang.li
     * @Date 16:41 2019/3/5
     **/
    ResponseResult<ResCodeEnum> switchOrg(UserInfo userInfo, Integer orgId);

    /**
     * @notes 将用户信息插入的缓存中
     * @Author junyang.li
     * @Date 11:08 2019/3/6
     **/
    void addToRedis(UserInfo userInfo);

    /**
     * @notes 删除员工
     * @Author junyang.li
     * @Date 14:28 2019/3/6
     **/
    ResponseResult<ResCodeEnum> deleteUserForDepartment(UserInfo userInfo, int id);

    /**
     * @notes 修改个人信息
     * @Author junyang.li
     * @Date 11:28 2019/3/14
     **/
    ResponseResult<ResCodeEnum> modifySelfInfo(UserInfo userInfo, EditUserRQ editUserRQ);

    /**
     * @notes 修改头像
     * @Author junyang.li
     * @Date 12:29 2019/3/14
     **/
    ResponseResult<ResCodeEnum> modifyHead(UserInfo userInfo, String head);

    /**
     * @notes 搜索用户
     * @Author junyang.li
     * @Date 13:50 2019/3/14
     **/
    ResponseResult<UserBasicDTO> findUserAndDepartment(UserInfo userInfo, String name);

    /**
     * @notes 供应链金融根据关键词查询用户
     * @Author junyang.li
     * @Date 16:05 2019/3/14
     **/
    ResponseResult<List<UserInfo>> getUserByKeyWord(String keyWord);

    /**
     * @notes 通过用户id获取用户信息
     * @Author junyang.li
     * @Date 17:29 2019/3/21
     **/
    ResponseResult<UserDetailDTO> getUserById(UserInfo userInfo, Integer userId);

    /**
     * @notes 内部服务获取指定用户的企业信息
     * @Author junyang.li
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
     * 查询用户管理列表显示
     * @param rq 条件
     * @param page
     * @return
     */
    ResponseResult<List<UserManagerListDTO>> getUserManagerList(UserManagerListRQ rq, Page page);

    /**
     * 查询用户列表 的用户详情
     * @param userId 用户id
     * @return
     */
    ResponseResult<UserListDetailDTO> getUserListDetail(Integer userId, ManagerInfo managerInfo);

    /**
     * 更新用户列表 的用户详情 信息
     * @param rq
     * @param managerInfo
     * @return
     */
    ResponseResult<ResCodeEnum> updateUserListDetail(UserListDetailRQ rq, ManagerInfo managerInfo);
}

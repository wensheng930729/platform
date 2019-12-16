package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.ManagerUsersDTO;
import com.bee.platform.user.vo.PlatformManagerEditVO;
import com.bee.platform.user.vo.PlatformManagerVO;

import java.util.List;

/**
 * @description:  管理后台的用户管理相关接口
 * @author: junyang.li
 * @create: 2019-04-25 14:11
 **/
public interface ManageUserService {

    /**
     * @notes 获得后台用户的登录信息
     * @Author junyang.li
     * @Date 10:37 2019/4/30
     **/
    ManagerInfo getManagerInfo(String sysToken);
    /**
     * @notes: 通过用户id获得用户详细信息
     * @Author: junyang.li
     * @Date: 11:15 2019/5/10
     * @param managerId :
     * @return: com.bee.platform.common.entity.ManagerInfo
     */
    ManagerInfo getManagerDetail(Integer managerId);
    /**
     * @notes  管理后台编辑用户
     * @Author junyang.li
     * @Date 14:13 2019/4/25
     **/
    ResponseResult<ResCodeEnum> editUser(ManagerInfo managerInfo, PlatformManagerVO vo);
    /**
     * @notes: 个人中心更换手机号获得手机验证码
     * @Author: junyang.li
     * @Date: 15:30 2019/5/5
     * @param managerInfo : 操作人信息
     * @param newPhone : 新的手机号
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> getPhoneCheckCode(ManagerInfo managerInfo,String newPhone);
    /**
     * @notes: 个人中心更换邮箱获得邮箱验证码
     * @Author: junyang.li
     * @Date: 15:30 2019/5/5
     * @param managerInfo : 操作人信息
     * @param newEmail : 新的手机号
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> getEmailCheckCode(ManagerInfo managerInfo,String newEmail);
    /**
     * @notes: 个人中心修改个人资料
     * @Author: junyang.li
     * @Date: 11:41 2019/5/5
     * @param managerInfo : 操作人信息
     * @param editVO : 修改参数
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    ResponseResult<ResCodeEnum> updateManager(ManagerInfo managerInfo, PlatformManagerEditVO editVO);
    /**
     * @notes: 将管理后台的用户信息加入到缓存中
     * @Author: junyang.li
     * @Date: 17:04 2019/5/5
     * @param managerInfo : 用户信息
     * @return: void
     */
    void  addManagerInfoToRedis(ManagerInfo managerInfo);

    /**
     * @notes: 权限-用户管理 获取用户列表
     * @Author: junyang.li
     * @Date: 11:46 2019/5/9
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.user.dto.ManagerUsersDTO>>
     */
    ResponseResult<List<ManagerUsersDTO>> getUserList(Pagination pagination);

    /**
     * @notes: 用户列表账户启禁用
     * @Author: junyang.li
     * @Date: 11:49 2019/5/9
     * @param managerInfo : 操作人id
     * @param managerId : 用户id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> editAccountStatus(ManagerInfo managerInfo,Integer managerId);
    /**
     * @notes: 管理员重置后台账号的密码
     * @Author: junyang.li
     * @Date: 13:59 2019/5/9
     * @param managerInfo : 操作人
     * @param managerId : 被操作人
     * @param type : 重置的方式
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> resetMemberPassword(ManagerInfo managerInfo,Integer managerId,Integer type);
}

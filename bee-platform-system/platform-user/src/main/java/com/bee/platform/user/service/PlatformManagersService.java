package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.entity.PlatformManagers;
import com.bee.platform.user.vo.PlatformManagerVO;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author qhwang123
 * @since 2019-03-14
 */
public interface PlatformManagersService extends IService<PlatformManagers> {


    /**
     * @notes: 通过用户id获得用户名称
     * @Author: junyang.li
     * @Date: 17:33 2019/4/30
     * @params: [ids, b]
     * @return: java.util.Map<java.lang.Integer,java.lang.String>
     **/
    Map<Integer,String> getManagerNameById(Set<Integer> ids);
    /**
     * 通过用户名查询管理员信息
     * @param userName
     * @return
     */
    PlatformManagers getManagerByName(String userName);

    /**
     * 更新密码
     * @param request
     * @param old_password
     * @param password
     * @return
     */
    ResponseResult updatePassword(HttpServletRequest request, String old_password, String password);

    /**
     * 更新密码
     * @param id
     * @param password
     * @return
     */
    ResponseResult updatePassword(int id, String password);

    /**
     * 获取管理员
     * @return
     */
    ResponseResult getAdmins();

    /**
     * 添加管理员
     * @param nickname
     * @param username
     * @param password
     * @return
     */
    ResponseResult addAdmin(String nickname, String username, String password);

    /**
     * 删除管理员
     * @param request
     * @param id
     * @return
     */
    ResponseResult deleteManager(HttpServletRequest request, int id);

    /**
     * 获取管理员nickname
     * @param request
     * @return
     */
    ResponseResult getName(HttpServletRequest request);

}

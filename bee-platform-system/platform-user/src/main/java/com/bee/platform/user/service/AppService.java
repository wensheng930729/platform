package com.bee.platform.user.service;


import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.AppDTO;
import com.bee.platform.user.entity.App;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * app 服务类
 * </p>
 *
 * @author chengke
 * @since 2019-03-18
 */
public interface AppService extends IService<App> {
    /**
     * 根据企业ID查询相关app列表
     * @param userInfo
     * @return
     */
    List<AppDTO> getAppsListByOrgId(AuthPlatformUserInfo userInfo);

    /**
     * 根据用户信息查询相关app列表
     * @param userInfo
     * @param userId
     * @return
     */
    List<AppDTO> getAppsList(AuthPlatformUserInfo userInfo, int userId);

    /**
     * 用户获取应用列表
     * @param userInfo
     * @return
     */
    List<AppDTO> getAppListByUser(AuthPlatformUserInfo userInfo);


    /**
     * 用户开通应用
     * @param userInfo
     * @param userIds
     * @param appIds
     * @return
     */
    ResponseResult addAppToUser(AuthPlatformUserInfo userInfo, int[] userIds, int[] appIds);


    /**
     * 移除用户应用
     * @param userInfo
     * @param userIds
     * @param appIds
     * @return
     */
    ResponseResult removeAppFromUser(AuthPlatformUserInfo userInfo, int[] userIds, int[] appIds);

    /**
     * 查询app产品列表
     * @return
     */
    List<AppDTO> getAllAppsList();

}

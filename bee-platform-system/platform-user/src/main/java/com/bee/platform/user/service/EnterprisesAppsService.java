package com.bee.platform.user.service;

import com.bee.platform.common.entity.*;
import com.bee.platform.user.dto.AppDetailDTO;
import com.bee.platform.user.dto.AppListDTO;
import com.bee.platform.user.dto.AppRolesDTO;
import com.bee.platform.user.dto.EnterprisesAppsDTO;
import com.bee.platform.user.entity.AppRoles;
import com.bee.platform.user.entity.EnterprisesApps;
import com.bee.platform.user.rq.AppDetailRQ;
import com.bee.platform.user.rq.EnterprisesAppsAuditRQ;
import com.bee.platform.user.rq.EnterprisesAppOpenRQ;
import com.bee.platform.user.rq.EnterprisesAppsRQ;

import java.util.List;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface EnterprisesAppsService extends IService<EnterprisesApps> {
	/**
	 * 条件查询申请列表
	 * @param rq
	 * @param pagination
	 * @return
	 */
	ResponseResult<List<EnterprisesAppsDTO>> getApplyList(EnterprisesAppsRQ rq, Pagination pagination);
	/**
	 * 审核产品角色申请
	 * @param rq
	 * @param userInfo 
	 * @return
	 */
	ResponseResult audit(EnterprisesAppsAuditRQ rq, AuthPlatformUserInfo userInfo);

	/**
	 * 开通产品、增加角色
	 * @param appDetailRQ
	 * @param userInfo
	 * @return
	 */
	ResponseResult<ResCodeEnum> openApp(AppDetailRQ appDetailRQ, AuthPlatformUserInfo userInfo);

	/**
	 * 获取企业产品信息-新开通
	 * @param id
	 * @param userInfo
	 * @return
	 */
	ResponseResult<AppDetailDTO> getInfoForOpenApp(String id, AuthPlatformUserInfo userInfo);

	/**
	 * 获取已开通产品角色
	 * @param userInfo
	 * @return
	 */
	//ResponseResult<List<AppRoles>> getNotOpenedRoles(UserInfo userInfo, String appId);

	/**
	 * 获取已开通产品列表-未开通角色
	 * @param userInfo
	 * @return
	 */
	ResponseResult<List<AppDetailDTO>> listOpenedAppNoRoles(AuthPlatformUserInfo userInfo);

	/**
	 * 获取已开通产品列表-已开通角色
	 * @param userInfo
	 * @return
	 */
	ResponseResult<List<AppDetailDTO>> listOpenedAppWithRoles(AuthPlatformUserInfo userInfo);

	/**
	 *
	 * @param userInfo
	 * @return
	 */
	//ResponseResult<List<AppRoles>> getOpenedRoles(UserInfo userInfo, String appId);

	/**
	 * 获取未开通产品列表
	 * @param userInfo
	 * @return
	 */
	ResponseResult<List<AppListDTO>> listNotOpenedApp(AuthPlatformUserInfo userInfo);
}

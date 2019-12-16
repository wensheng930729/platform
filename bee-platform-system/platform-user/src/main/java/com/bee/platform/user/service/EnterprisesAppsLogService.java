package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.entity.EnterprisesAppsLog;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author xu.zheng123
 * @since 2019-04-29
 */
public interface EnterprisesAppsLogService extends IService<EnterprisesAppsLog> {

	/**
	 * 前中台使用
	 *  添加产品及角色日志记录
	 * @param enterprisesAppsId 产品及角色申请id
	 * @param operateType 操作类型
	 * @param executResult 执行结果
	 * @param refuseReason 拒绝原因
	 * @param userInfo 用户
	 * @return
	 */
	Integer insertEnterprisesAppsLog(Integer enterprisesAppsId, String operateType, String executResult,String refuseReason,
			UserInfo userInfo);

	/**
	 * 中台获取产品角色审批列表
	 * @param userInfo
	 * @return
	 */
    List<EnterprisesAppsLog> listEnterprisesAppsLog(AuthPlatformUserInfo userInfo, Pagination pagination);

    /**
     * 前中台使用
     *  添加产品及角色日志记录
     * @param enterprisesAppsId 产品及角色申请id
     * @param operateType 操作类型
     * @param executResult 执行结果
     * @param refuseReason 拒绝原因
     * @param userInfo 用户
     * @return
     */
    Integer insertEnterprisesAppsLog(Integer enterprisesAppsId, String operateType, String executResult, String refuseReason,
									 AuthPlatformUserInfo userInfo);
}

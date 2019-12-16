package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.dto.EnterprisesCheckLogDTO;
import com.bee.platform.user.entity.EnterprisesCheckLog;

import java.util.List;

/**
 * <p>
 * 企业审核日志表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-04-30
 */
public interface EnterprisesCheckLogService extends IService<EnterprisesCheckLog> {

	/**
	 * 查询用户企业申请审核日志信息
	 * 
	 * @param userInfo 用户信息
	 * @param page     分页条件
	 * @return 审核日志信息
	 */
	ResponseResult<List<EnterprisesCheckLogDTO>> getCheckLogs(AuthPlatformUserInfo userInfo, Page page);

	/**
	 * 后台使用记录企业认证日志
	 * 
	 * @param operateType
	 * @param executResult
	 * @param refuseReason
	 * @param userInfo
	 * @param user 
	 * @return
	 */
	Integer insertEnterprisesCheckLog(Integer enterprisesCheckId, Integer operateType, Integer executResult,
			String refuseReason, AuthPlatformUserInfo userInfo, AuthPlatformUser user);

}

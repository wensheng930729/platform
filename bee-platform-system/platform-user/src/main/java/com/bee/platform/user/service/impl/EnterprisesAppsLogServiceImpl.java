package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.user.dao.mapper.AppMapper;
import com.bee.platform.user.dao.mapper.AppRolesMapper;
import com.bee.platform.user.dao.mapper.EnterprisesAppsLogMapper;
import com.bee.platform.user.dao.mapper.EnterprisesAppsMapper;
import com.bee.platform.user.entity.App;
import com.bee.platform.user.entity.AppRoles;
import com.bee.platform.user.entity.EnterprisesApps;
import com.bee.platform.user.entity.EnterprisesAppsLog;
import com.bee.platform.user.service.EnterprisesAppsLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author xu.zheng123
 * @since 2019-04-29
 */
@Slf4j
@Service
public class EnterprisesAppsLogServiceImpl extends ServiceImpl<EnterprisesAppsLogMapper, EnterprisesAppsLog> implements EnterprisesAppsLogService {

	@Autowired
	private EnterprisesAppsMapper enterprisesAppsMapper;
	@Autowired
	private EnterprisesAppsLogMapper enterprisesAppsLogMapper;
	@Autowired
	private AppMapper appMapper;
	@Autowired
	private AppRolesMapper appRolesMapper;
	
	@Override
	public Integer insertEnterprisesAppsLog(Integer enterprisesAppsId, String operateType, String executResult,
			String refuseReason, UserInfo userInfo) {
		EnterprisesAppsLog enterprisesAppsLog = new EnterprisesAppsLog();
		EnterprisesApps eApps = enterprisesAppsMapper.selectOne(new EnterprisesApps().setId(enterprisesAppsId));
		enterprisesAppsLog.setEnterprisesAppsId(eApps.getId());
		enterprisesAppsLog.setOrgId(eApps.getOrgId());
		enterprisesAppsLog.setOrgName(eApps.getOrgName());
		enterprisesAppsLog.setAppId(eApps.getAppId());
		App app = appMapper.selectById(eApps.getAppId());
		enterprisesAppsLog.setAppName(app.getName());
		enterprisesAppsLog.setAppRolesId(eApps.getAppRolesId());
		AppRoles appRoles = appRolesMapper.selectById(eApps.getAppRolesId());
		enterprisesAppsLog.setAppRolesName(appRoles.getRoleName());
		enterprisesAppsLog.setOperateType(operateType);
		enterprisesAppsLog.setAuditResult(executResult);
		enterprisesAppsLog.setRejectReason(refuseReason);
		enterprisesAppsLog.setStatus(Status.TRUE.getKey());
		enterprisesAppsLog.setAuditorId(userInfo.getId());
		enterprisesAppsLog.setAuditor(userInfo.getNickname());
		enterprisesAppsLog.setAuditTime(new Date());
		Integer count = enterprisesAppsLogMapper.insert(enterprisesAppsLog);
		if (count <= 0) {
			log.error("插入日志记录失败！调用{}的{}方法出错 ", "EnterprisesAppsLogServiceImpl", "insertEnterprisesAppsLog()");
			throw new BusinessException(ResCodeEnum.UPDATA_FAIL, ExceptionMessageEnum.ENTERPRISESAPPSLOG_SAVE_FAILED);
		}
		return count;
	}

	/**
	 * 中台获取产品角色审批列表
	 * @param userInfo
	 * @return
	 */
	@Override
	public List<EnterprisesAppsLog> listEnterprisesAppsLog(AuthPlatformUserInfo userInfo, Pagination pagination) {
		return enterprisesAppsLogMapper.selectPage(pagination,new EntityWrapper<EnterprisesAppsLog>()
				.eq("org_id",userInfo.getOrgId()).and()
				.eq("status",Status.TRUE.getKey())
				.orderBy("audit_time",false));
	}

	@Override
	public Integer insertEnterprisesAppsLog(Integer enterprisesAppsId, String operateType, String executResult,
			String refuseReason, AuthPlatformUserInfo managerInfo) {
		EnterprisesAppsLog enterprisesAppsLog = new EnterprisesAppsLog();
		EnterprisesApps eApps = enterprisesAppsMapper.selectOne(new EnterprisesApps().setId(enterprisesAppsId));
		enterprisesAppsLog.setEnterprisesAppsId(eApps.getId());
		enterprisesAppsLog.setOrgId(eApps.getOrgId());
		enterprisesAppsLog.setOrgName(eApps.getOrgName());
		enterprisesAppsLog.setAppId(eApps.getAppId());
		App app = appMapper.selectById(eApps.getAppId());
		enterprisesAppsLog.setAppName(app.getName());
		enterprisesAppsLog.setAppRolesId(eApps.getAppRolesId());
		AppRoles appRoles = appRolesMapper.selectById(eApps.getAppRolesId());
		enterprisesAppsLog.setAppRolesName(appRoles.getRoleName());
		enterprisesAppsLog.setOperateType(operateType);
		enterprisesAppsLog.setAuditResult(executResult);
		enterprisesAppsLog.setRejectReason(refuseReason);
		enterprisesAppsLog.setStatus(Status.TRUE.getKey());
		enterprisesAppsLog.setAuditorId(managerInfo.getId());
		enterprisesAppsLog.setAuditor(managerInfo.getNickname());
		enterprisesAppsLog.setAuditTime(new Date());
		Integer count = enterprisesAppsLogMapper.insert(enterprisesAppsLog);
		if (count <= 0) {
			log.error("插入日志记录失败！调用{}的{}方法出错 ", "EnterprisesAppsLogServiceImpl", "insertEnterprisesAppsLog()");
			throw new BusinessException(ResCodeEnum.UPDATA_FAIL, ExceptionMessageEnum.ENTERPRISESAPPSLOG_SAVE_FAILED);
		}
		return count;
	}

}

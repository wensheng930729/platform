package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.constants.enums.EnumOperateLog;
import com.bee.platform.common.constants.enums.EnumSearchMode;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.AuditStateEnum;
import com.bee.platform.common.enums.EnumEnterpriseApp;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.MiddleNoticeUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dao.mapper.*;
import com.bee.platform.user.dto.AppDetailDTO;
import com.bee.platform.user.dto.AppListDTO;
import com.bee.platform.user.dto.AppRolesDTO;
import com.bee.platform.user.dto.EnterprisesAppsDTO;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.rq.AppDetailRQ;
import com.bee.platform.user.rq.EnterprisesAppsAuditRQ;
import com.bee.platform.user.rq.EnterprisesAppsRQ;
import com.bee.platform.user.rq.MiddleSystemNoticeRQ;
import com.bee.platform.user.service.*;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.ValidateUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
@Slf4j
@Service
public class EnterprisesAppsServiceImpl extends ServiceImpl<EnterprisesAppsMapper, EnterprisesApps>
		implements EnterprisesAppsService {
	@Autowired
	private EnterprisesAppsMapper enterprisesAppsMapper;
	@Autowired
	private AppMapper appMapper;
	@Autowired
	private AppRolesMapper appRolesMapper;
	@Autowired
	private EnterprisesAppsLogService enterprisesAppsLogService;
	@Autowired
	private SmsService smsService;
	@Autowired
	private EnterprisesMapper enterprisesMapper;
	@Autowired
	private UserMapper userMapper;
	@Autowired
	private MiddleSystemNoticeService middleSystemNoticeService;
	@Autowired
	private SystemNoticeService systemNoticeService;
	@Autowired
	private MPlatformManagersMapper mPlatformManagersMapper;
	@Autowired
	private ConfigService configService;
	@Autowired
	private MRoleRoleMapper mRoleRoleMapper;
	@Autowired
	private OperatorLogService operatorLogService;

	private static Integer ZERO = 0;

	@Override
	public ResponseResult<List<EnterprisesAppsDTO>> getApplyList(EnterprisesAppsRQ rq, Pagination pagination) {
		Integer auditState = rq.getAuditState();
		// 全部模式，刚加载的时候使用
		if (EnumSearchMode.SearchMode.all.getKey().equals(rq.getMode())) {
			List<EnterprisesAppsDTO> dtos = getApplyListByOrgName(AuditStateEnum.ALL.getKey(), null, pagination);
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtos, PageUtils.transToPage(pagination));
		}
		// 企业名称搜索模式
		if (EnumSearchMode.SearchMode.enterprise.getKey().equals(rq.getMode())) {
			List<EnterprisesAppsDTO> dtos = getApplyListByOrgName(auditState, rq.getContent(), pagination);
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtos, PageUtils.transToPage(pagination));
		}
		// 产品名称搜索
		if (EnumSearchMode.SearchMode.product.getKey().equals(rq.getMode())) {
			List<EnterprisesAppsDTO> dtos = getApplyListByAppName(auditState, rq.getContent(), pagination);
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtos, PageUtils.transToPage(pagination));
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	}

	/**
	 * 开通产品、增加角色
	 *
	 * @param appDetailRQ
	 * @param userInfo
	 * @return
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<ResCodeEnum> openApp(AppDetailRQ appDetailRQ, AuthPlatformUserInfo userInfo) {
		StringBuilder role = new StringBuilder();
        for (int i = 0; i < appDetailRQ.getAppRoleList().size(); i++) {
            role.append("," + appDetailRQ.getAppRoleList().get(i).getRoleName());
            EnterprisesApps enterprisesApps = new EnterprisesApps()
                    .setOrgId(userInfo.getOrgId())
                    .setOrgName(userInfo.getOrg_name())
                    .setAppId(appDetailRQ.getId())
                    .setAppRolesId(appDetailRQ.getAppRoleList().get(i).getId())
                    .setAduitState(AuditStateEnum.ON_AUDIT.getKey())
                    .setStatus(Status.TRUE.getKey())
                    .setCreateId(userInfo.getId())
                    .setCreator(userInfo.getNickname())
                    .setCreateTime(new Date());
            // 插入企业产品信息
            if (enterprisesAppsMapper.insert(enterprisesApps) <= ZERO) {
                log.error("调用{}的保存方法出错，产品开通保存异常!", "openApp");
                throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
            }
            // 插入企业产品开通审核记录
            if (enterprisesAppsLogService.insertEnterprisesAppsLog(enterprisesApps.getId(),
                    EnumOperateLog.AppOperateType.apply.getValue(),
                    EnumOperateLog.executResult.ON_AUDIT.getValue(),
                    null,
                    userInfo) <= ZERO) {
                log.error("调用{}的保存方法出错，产品开通申请记录保存异常!", "openApp");
                throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
            }
        }
        // 保存中台消息
        MiddleSystemNoticeRQ middleSystemNoticeRQ = new MiddleSystemNoticeRQ();
        middleSystemNoticeService.createNotice(userInfo.getId(),EnumMiddleNoticeTitle.title.OPEN_PRODUCT_APPLY.getValue(),
                MiddleNoticeContentTemplate.PRODUCT_OPENING_APPLY.getKey(),appDetailRQ.getName(),role);

        //保存后台消息
        List<Integer> roles = mRoleRoleMapper.getManagerIdsByChildId(9);
        int arr[] = roles.stream().mapToInt(Integer::byteValue).toArray();
        List<SystemNotice> systemNotices = systemNoticeService.createNotice(arr, NoticeTemplateType.PRODUCT_OPEN_APPLY,
                userInfo.getOrg_name(), appDetailRQ.getName(),appDetailRQ.getName(),role);

        // 插入系统通知
        try {
            systemNoticeService.insertAll(systemNotices);
        }catch (Exception e){
            log.error("新增后台系统通知信息失败,异常信息{}",e);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

	/**
	 * 获取企业产品信息-新开通
	 *
	 * @param id
	 * @param userInfo
	 * @return
	 */
	@Override
	public ResponseResult<AppDetailDTO> getInfoForOpenApp(String id, AuthPlatformUserInfo userInfo) {
		// 获取产品信息
		App app = appMapper.selectById(id);
		AppDetailDTO appDetail = BeanUtils.copyProperties(app, AppDetailDTO.class);
		// 获取产品角色信息
		List<AppRoles> appRoles = appRolesMapper.selectList(new EntityWrapper<AppRoles>().eq("app_id", id));
		List<AppRolesDTO> appRoleDTOS = BeanUtils.assemble(AppRolesDTO.class, appRoles);
		//是否有待审核的产品角色
		for (AppRolesDTO roles : appRoleDTOS){
			if (!CollectionUtils.isEmpty(enterprisesAppsMapper.selectList(new EntityWrapper<EnterprisesApps>()
					.eq("org_id",userInfo.getOrgId()).and()
					.eq("app_id",id).and()
					.eq("status",Status.TRUE.getKey()).and()
					.eq("aduit_state",AuditStateEnum.ON_AUDIT.getKey())))){
				roles.setAuditStatus(AuditStateEnum.ON_AUDIT.getKey());
			}else if(!CollectionUtils.isEmpty(enterprisesAppsMapper.selectList(new EntityWrapper<EnterprisesApps>()
					.eq("org_id",userInfo.getOrgId()).and()
					.eq("app_id",id).and()
					.eq("status",Status.TRUE.getKey()).and()
					.eq("aduit_state",AuditStateEnum.PASSED.getKey())))){
				roles.setAuditStatus(AuditStateEnum.PASSED.getKey());
			}else{
				roles.setAuditStatus(AuditStateEnum.NOT_PASSED.getKey());
			}
		}
		if (!CollectionUtils.isEmpty(enterprisesAppsMapper.selectList(new EntityWrapper<EnterprisesApps>()
				.eq("org_id",userInfo.getOrgId()).and()
				.eq("app_id",id).and()
				.eq("status",Status.TRUE.getKey()).and()
				.eq("aduit_state",AuditStateEnum.ON_AUDIT.getKey())))){
			appDetail.setEditStatu(Status.FALSE.getKey());
		}


		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appDetail.setAppRoleList(appRoleDTOS));

	}

	/**
	 * 根据产品名称查找申请列表
	 * 
	 * @param auditState
	 * @param content
	 * @param pagination
	 * @return
	 */
	private List<EnterprisesAppsDTO> getApplyListByAppName(Integer auditState, String content, Pagination pagination) {
		if (AuditStateEnum.ALL.getKey().equals(auditState)) {
			List<EnterprisesAppsDTO> allList = enterprisesAppsMapper.getAllByAppName(content, pagination);
			return allList;
		}
		List<EnterprisesAppsDTO> lists = enterprisesAppsMapper.queryAppsByAppName(auditState, content, pagination);
		return lists;
	}

	/**
	 * 根据公司名称查找申请列表
	 * 
	 * @param auditState
	 * @param content
	 * @param pagination
	 * @return
	 */
	private List<EnterprisesAppsDTO> getApplyListByOrgName(Integer auditState, String content, Pagination pagination) {
		if (AuditStateEnum.ALL.getKey().equals(auditState)) {
			List<EnterprisesAppsDTO> allList = enterprisesAppsMapper.getAllByOrgName(content, pagination);
			return allList;
		}
		List<EnterprisesAppsDTO> lists = enterprisesAppsMapper.queryAppsByOrgName(auditState, content, pagination);
		return lists;
	}

	/**
	 * 审核产品角色
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult audit(EnterprisesAppsAuditRQ rq, AuthPlatformUserInfo managerInfo) {
		EnterprisesApps eApps = enterprisesAppsMapper
				.selectOne(new EnterprisesApps().setId(rq.getId()).setStatus(Status.TRUE.getKey()));
		eApps.setAduitState(rq.getAuditState());
		// 产品名称
		String appName = appMapper.selectById(eApps.getAppId()).getName();
		// 角色名称
		String appRoleName = appRolesMapper.selectById(eApps.getAppRolesId()).getRoleName();
		// 审核通过，则将退回原因清空
		/* 后台消息记录通知人 */
		String childRoleId = configService.getConfigByconfigKey("appRoles_audit_roleId").getConfigValue();
		List<Integer> managers = mRoleRoleMapper.getManagerIdsByChildId(Integer.valueOf(childRoleId));
		int[] managerIds = managers.stream().mapToInt(Integer::valueOf).toArray();
		/* 后台消息内容 */
		List<SystemNotice> systemNotices = new ArrayList<SystemNotice>();
		// 后台展示的日志对象
		OperatorLog operatorLog = new OperatorLog();
		if (AuditStateEnum.PASSED.getKey().equals(rq.getAuditState())) {
			eApps.setRejectReason(null);
			// 添加审核日志
			enterprisesAppsLogService.insertEnterprisesAppsLog(eApps.getId(),
					EnumOperateLog.AppOperateType.audit.getValue(), EnumOperateLog.executResult.PASSED.getValue(), null,
					managerInfo);
			/* 后台消息内容 */
			systemNotices = systemNoticeService.createNotice(managerIds, NoticeTemplateType.PRODUCT_OPEN_APPROVAL,
					EnumEnterpriseApp.CheckMsgType.PASSED.getValue(), eApps.getOrgName(), appName, appRoleName);
			// 添加后台展示的日志
			operatorLog.setOperatorContent(AuditStateEnum.PASSED.getValue() + eApps.getOrgName() + " " + appName + "|"
					+ appRoleName + "的开通申请");
		} else {
			eApps.setRejectReason(rq.getRejectReason());
			// 添加审核日志
			enterprisesAppsLogService.insertEnterprisesAppsLog(eApps.getId(),
					EnumOperateLog.AppOperateType.audit.getValue(), EnumOperateLog.executResult.NOT_PASSED.getValue(),
					rq.getRejectReason(), managerInfo);
			/* 后台消息记录 */
			systemNotices = systemNoticeService.createNotice(managerIds, NoticeTemplateType.PRODUCT_OPEN_APPROVAL,
					EnumEnterpriseApp.CheckMsgType.REFUSED.getValue(), eApps.getOrgName(), appName, appRoleName);
			// 添加后台展示的日志
			operatorLog.setOperatorContent(AuditStateEnum.NOT_PASSED.getValue() + eApps.getOrgName() + " " + appName
					+ "|" + appRoleName + "的开通申请");
		}
		if (enterprisesAppsMapper.updateById(eApps) <= ZERO) {
			log.error("产品及角色申请审核信息失败！调用{}的{}方法出错 ", "EnterprisesAppsServiceImpl", "audit()");
			throw new BusinessException(ResCodeEnum.UPDATA_FAIL, ExceptionMessageEnum.USER_OPEN_APP_FAILED);
		}
		// 插入后台消息
		if (!systemNoticeService.insertBatch(systemNotices)) {
			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
		}
		// 后台日志信息
		/*operatorLog = new OperatorLog().setOperatorId(managerInfo.getManagerId())
				.setOperatorRoleId(managerInfo.getRoleInfo().getRoleId())
				.setOperatorRoleName(managerInfo.getRoleInfo().getRoleName()).setOperatorTime(new Date());*/

		operatorLogService.insert(operatorLog);
		/* 中台消息记录及短信 */
		if (AuditStateEnum.PASSED.getKey().equals(rq.getAuditState())) {
			this.sendSmsAfterAudit(eApps, NoticeTemplateType.PRODUCT_OPENED_AUDIT_APPROVED.getKey());
		} else {
			this.sendSmsAfterAudit(eApps, NoticeTemplateType.PRODUCT_OPENED_AUDIT_FAILED.getKey());
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
	}

	/**
	 * 获取已开通产品列表-未开通角色
	 *
	 * @param userInfo
	 * @return
	 */
	@Override
	public ResponseResult<List<AppDetailDTO>> listOpenedAppNoRoles(AuthPlatformUserInfo userInfo) {
		// 查询已开通产品
		List<AppListDTO> appListDTOS = enterprisesAppsMapper.listOpenedApp(userInfo.getOrgId());
		if (CollectionUtils.isEmpty(appListDTOS)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<AppDetailDTO>());
		}
		// 查询已开通产品已未通角色
		List<AppDetailDTO> appDetailDTOS = new ArrayList<AppDetailDTO>();
		for (int i = 0; i < appListDTOS.size(); i++) {
			AppDetailDTO detailDTO = new AppDetailDTO();
			detailDTO.setId(appListDTOS.get(i).getId());
			detailDTO.setName(appListDTOS.get(i).getName());
			detailDTO.setAbbreviation(appListDTOS.get(i).getAbbreviation());
			// 查询产品已开通角色
			List<AppRoles> appRoles = appRolesMapper
					.selectList(new EntityWrapper<AppRoles>().eq("app_id", appListDTOS.get(i).getId()));
			List<AppRolesDTO> appRolesDTOList = BeanUtils.assemble(AppRolesDTO.class, appRoles);
			// 查询产品角色审核状态
			for (int j = 0; j < appRolesDTOList.size(); j++) {
				if (!CollectionUtils.isEmpty(enterprisesAppsMapper.selectList(new EntityWrapper<EnterprisesApps>()
						.eq("org_id", userInfo.getOrgId()).and().eq("app_id", appListDTOS.get(i).getId()).and()
						.eq("app_roles_id", appRolesDTOList.get(j).getId()).and()
						.eq("aduit_state", AuditStateEnum.ON_AUDIT.getKey())))) {
					appRolesDTOList.get(j).setAuditStatus(AuditStateEnum.ON_AUDIT.getKey());
					detailDTO.setEditStatu(Status.FALSE.getKey());
				} else if (!CollectionUtils
						.isEmpty(enterprisesAppsMapper.selectList(new EntityWrapper<EnterprisesApps>()
								.eq("org_id", userInfo.getOrgId()).and().eq("app_id", appListDTOS.get(i).getId()).and()
								.eq("app_roles_id", appRolesDTOList.get(j).getId()).and()
								.eq("aduit_state", AuditStateEnum.PASSED.getKey())))) {
					appRolesDTOList.get(j).setAuditStatus(AuditStateEnum.PASSED.getKey());
				} else {
					appRolesDTOList.get(j).setAuditStatus(AuditStateEnum.NOT_PASSED.getKey());
				}
			}
			detailDTO.setAppRoleList(appRolesDTOList);
			appDetailDTOS.add(detailDTO);
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appDetailDTOS);
	}

	/**
	 * 获取已开通产品列表-已开通角色
	 *
	 * @param userInfo
	 * @return
	 */
	@Override
	public ResponseResult<List<AppDetailDTO>> listOpenedAppWithRoles(AuthPlatformUserInfo userInfo) {
		// 查询已开通产品
		List<AppListDTO> appListDTOS = enterprisesAppsMapper.listOpenedApp(userInfo.getOrgId());
		if (CollectionUtils.isEmpty(appListDTOS)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<AppDetailDTO>());
		}
		// 查询已开通产品已开通角色
		List<AppDetailDTO> appDetailDTOS = new ArrayList<AppDetailDTO>();
		for (int i = 0; i < appListDTOS.size(); i++) {
			AppDetailDTO detailDTO = new AppDetailDTO();
			detailDTO.setId(appListDTOS.get(i).getId());
			detailDTO.setName(appListDTOS.get(i).getName());
			detailDTO.setAbbreviation(appListDTOS.get(i).getAbbreviation());
			// 查询产品已开通角色
			List<AppRoles> appRoles = appRolesMapper.selectOpenedRoles(appListDTOS.get(i).getId(), userInfo.getOrgId());
			detailDTO.setAppRoleList(BeanUtils.assemble(AppRolesDTO.class, appRoles));
			appDetailDTOS.add(detailDTO);
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appDetailDTOS);
	}

	/**
	 * 获取未开通产品列表
	 *
	 * @param userInfo
	 * @return
	 */
	@Override
	public ResponseResult<List<AppListDTO>> listNotOpenedApp(AuthPlatformUserInfo userInfo) {
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
				enterprisesAppsMapper.listNotOpenedApp(userInfo.getOrgId()));
	}

	/**
	 * @Description 产品开通审批后发送短信
	 * @author chenxm66777123
	 * @Date 2019年5月9日
	 * @version 1.0.0
	 */
	private void sendSmsAfterAudit(EnterprisesApps eApps, Integer key) {
		String phone = "";
		try {
			// 企业管理员 您提交的开通产品${product_name} ；角色${role_name}的申请审核已通过，可在产品开通页面查看详情。
			// 查询企业管理员,获取企业信息
			Enterprises enterprises = enterprisesMapper
					.selectOne(new Enterprises().setId(eApps.getOrgId()).setStatus(Status.TRUE.getKey()));
			if (ObjectUtils.isEmpty(enterprises) && ObjectUtils.isEmpty(enterprises.getAdmin())) {
				log.info(ResCodeEnum.APP_NOT_FIND.msg);
			}
			phone = enterprises.getAdmin();
			if (!ValidateUtils.isPhone(phone)) {
				log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
			}
			// 组装参数
			JSONObject sendJson = new JSONObject();
			// 开通产品
			App app = appMapper.selectOne(new App().setId(eApps.getAppId()));
			if (ObjectUtils.isEmpty(app)) {
				log.info(ResCodeEnum.APP_NOT_FIND.msg);
			}
			AppRoles appRoles = appRolesMapper.selectOne(new AppRoles().setId(eApps.getAppRolesId().longValue()));
			if (ObjectUtils.isEmpty(appRoles)) {
				log.info(ResCodeEnum.APP_ROLE_NOT_FIND.msg);
			}
			sendJson.put("product_name", app.getName());
			sendJson.put("role_name", appRoles.getRoleName());
			smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());

			// 保存发送消息
			MiddleSystemNoticeRQ middleSystemNoticeRQ = new MiddleSystemNoticeRQ();
			User user = userMapper.selectOne(new User().setUsername(phone));
			middleSystemNoticeRQ.setNotifierId(user.getId());
			middleSystemNoticeRQ.setTitle(EnumMiddleNoticeTitle.title.OPEN_PRODUCT_RESULT.getValue());
			if (NoticeTemplateType.PRODUCT_OPENED_AUDIT_APPROVED.getKey() == key) {
				String content = MiddleNoticeUtils.getValue(
						MiddleNoticeContentTemplate.PRODUCT_OPENING_APPLY_PASS.getKey(),
						new Object[] { app.getName(), appRoles.getRoleName() });
				middleSystemNoticeRQ.setContent(content);
			} else {
				String content = MiddleNoticeUtils.getValue(
						MiddleNoticeContentTemplate.PRODUCT_OPENING_APPLY_REFUSE.getKey(),
						new Object[] { app.getName(), appRoles.getRoleName() });
				middleSystemNoticeRQ.setContent(content);
			}

			middleSystemNoticeService.saveNotice(middleSystemNoticeRQ);

		} catch (Exception e) {
			log.info(e.getMessage());
		}
	}

}

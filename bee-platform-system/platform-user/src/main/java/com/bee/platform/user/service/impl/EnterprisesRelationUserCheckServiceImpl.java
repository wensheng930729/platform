package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.RoleType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.constants.enums.EnumEnterpriseRelationUserCheck;
import com.bee.platform.user.dao.mapper.EnterprisesMapper;
import com.bee.platform.user.dao.mapper.EnterprisesRelationUserCheckLogMapper;
import com.bee.platform.user.dao.mapper.EnterprisesRelationUserCheckMapper;
import com.bee.platform.user.dao.mapper.UserMapper;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckAmendRQ;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckDTO;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckDetailsDTO;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckRQ;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.rq.EnterprisesRelationUserRQ;
import com.bee.platform.user.rq.MiddleSystemNoticeRQ;
import com.bee.platform.user.service.*;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.ValidateUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.bee.platform.common.utils.ConstantsUtil.SYS_TOKEN;

/**
 * <p>
 * 企业关联用户审核表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */

@Slf4j
@Service
public class EnterprisesRelationUserCheckServiceImpl extends ServiceImpl<EnterprisesRelationUserCheckMapper, EnterprisesRelationUserCheck> implements EnterprisesRelationUserCheckService {

	@Autowired
	private UsersService usersService;

    @Autowired
    private EnterprisesService enterprisesService;

    @Autowired
    private EnterprisesRelationUserCheckLogMapper enterprisesRelationUserCheckLogMapper;
    
    @Autowired
    private EnterprisesRelationUserCheckMapper enterprisesRelationUserCheckMapper;
    
    @Autowired
    private UserMapper userMapper;
    
    @Autowired
    private EnterprisesMapper enterprisesMapper;
    @Autowired
	private EnterprisesUsersService enterprisesUsersService;

    @Autowired
    private SmsService smsService;
    @Autowired
	private MiddleSystemNoticeService middleSystemNoticeService;

	@Autowired
	private UsersRolesService usersRolesService;

	@Autowired
	private DepartmentsService departmentsService;
    /**
     * 企业关联用户
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 申请结果
     */
	@Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult enterprisesRelationUser(AuthPlatformUserInfo userInfo, EnterprisesRelationUserRQ rq) {
        Integer userId = userInfo.getId();
        String userName = userInfo.getNickname();
        String phone = userInfo.getPhone();
        String enterpriseName = rq.getEnterpriseName().replaceAll("\\s*", "");
        String applyReason = rq.getApplyReason();
        Date time = new Date();
		// 查询企业信息
		List<Enterprises> enterprises = enterprisesService.selectList(new EntityWrapper<Enterprises>().eq("name", enterpriseName));
		// 校验企业是否存在
		if(CollectionUtils.isEmpty(enterprises)){
			log.info("企业不存在");
			return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST, EnumEnterpriseCheck.checkRelationCompany.ENTERPRISE_NON_EXIST.getKey());
		}
		// 企业id
		Integer enterprisesId = enterprises.get(0).getId();
		// 获取用户关联过的企业id集合
		List<Integer> ids = enterprisesUsersService.selectList(new EntityWrapper<EnterprisesUsers>().eq("user_id", userId))
				.stream().map(EnterprisesUsers::getEnterpriseId).collect(Collectors.toList());
		// 用户已提交过该企业 审核已通过的
		EnterprisesRelationUserCheck one = this.selectOne(new EntityWrapper<EnterprisesRelationUserCheck>()
				.eq("enterprise_name",enterpriseName)
				.eq("user_id",userId)
				.eq("check_status", EnumEnterpriseRelationUserCheck.CheckType.PASSED.getKey())
				.eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
		// 校验用户是否已关联该企业
		if(ids.contains(enterprisesId)||!ObjectUtils.isEmpty(one)){
			log.info("用户已关联该企业");
			return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_ALREADY_ASSOCIATED,EnumEnterpriseCheck.checkRelationCompany.ENTERPRISE_ALREADY_ASSOCIATED.getKey());
		}
		// 用户已提交过该企业 未审核的
        EnterprisesRelationUserCheck two = this.selectOne(new EntityWrapper<EnterprisesRelationUserCheck>()
						.eq("enterprise_name",enterpriseName)
						.eq("user_id",userId)
						.eq("check_status",EnumEnterpriseRelationUserCheck.CheckType.IN_AUDIT.getKey())
						.eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
		if(!ObjectUtils.isEmpty(two)){
			log.info("用户已提交过该企业的关联申请，正在审核中，不可重复提交");
			return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_ASSOCIATED_IN_AUDIT,EnumEnterpriseCheck.checkRelationCompany.ENTERPRISE_ASSOCIATED_IN_AUDIT.getKey());
		}
		// 用户已提交过该企业 被拒绝的
		EnterprisesRelationUserCheck three = this.selectOne(new EntityWrapper<EnterprisesRelationUserCheck>()
				.eq("enterprise_name",enterpriseName)
				.eq("user_id",userId)
				.eq("check_status",EnumEnterpriseRelationUserCheck.CheckType.REFUSED.getKey())
				.eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
		// 校验是未通过重新提交还是新申请
		if(ObjectUtils.isEmpty(three)){
            // 新生成记录
            EnterprisesRelationUserCheck enterprisesRelationUserCheck = new EnterprisesRelationUserCheck()
                    .setUserId(userId).setUserName(userName).setPhone(phone).setEnterpriseId(enterprisesId)
                    .setEnterpriseName(enterpriseName).setApplyReason(applyReason).setCreateId(userId)
                    .setCreator(userName).setCreateTime(time).setModifyId(userId).setModifier(userName)
                    .setModifyTime(time).setCheckStatus(EnumEnterpriseRelationUserCheck.CheckType.IN_AUDIT.getKey());
            if(!insert(enterprisesRelationUserCheck)){
				log.error("用户关联企业申请信息保存失败！调用{}的{}方法出错 ", "EnterprisesRelationUserCheckServiceImpl", "enterprisesRelationUser()");
				throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_ASSOCIATED_SAVE_FAILED);
			}
            // 保存操作日志
			Integer checkId = enterprisesRelationUserCheck.getId();
			this.saveRelationCheckLog(userId, userName, enterpriseName, time, enterprisesId, checkId);

		}else {
            // 已有记录 被拒绝后 重新修改提交
			three.setApplyReason(applyReason)
                    .setCheckStatus(EnumEnterpriseRelationUserCheck.CheckType.IN_AUDIT.getKey())
                    .setModifyId(userId).setModifier(userName).setModifyTime(time).setRefusalReason(null);
            if(!updateById(three)){
				log.error("用户关联企业申请信息保存失败！调用{}的{}方法出错 ", "EnterprisesRelationUserCheckServiceImpl", "enterprisesRelationUser()");
				throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ENTERPRISE_ASSOCIATED_UPDATE_FAILED);
			}
            // 保存操作日志
			Integer checkId = three.getId();
			this.saveRelationCheckLog(userId, userName, enterpriseName, time, enterprisesId, checkId);

        }
		// 保存企业申请中台系统通知消息
		String title = EnumMiddleNoticeTitle.title.ENTERPRISE_RELATION_APPLY.getValue();
		middleSystemNoticeService.createNotice(userId,title,MiddleNoticeContentTemplate.ENTERPRISE_RELATE_APPLY.getKey(),enterpriseName);

		return ResponseResult.success("提交申请成功！请耐心等待审批！");
    }

	/**
	 * 保存操作日志
	 * @param userId 用户id
	 * @param userName 用户名
	 * @param enterpriseName 企业名称
	 * @param time 时间
	 * @param enterprisesId 企业id
	 * @param checkId checkId
	 */
	private void saveRelationCheckLog(Integer userId, String userName, String enterpriseName, Date time, Integer enterprisesId, Integer checkId) {
		EnterprisesRelationUserCheckLog log = new EnterprisesRelationUserCheckLog()
				.setEnterpriseRelationUserCheckId(checkId)
				.setCreateId(userId)
				.setCreator(userName)
				.setCreateTime(time)
				.setModifyId(userId)
				.setModifier(userName)
				.setModifyTime(time)
				.setOperateId(userId)
				.setOperateName(userName)
				.setOperateTime(time)
				.setOperateType(EnumEnterpriseRelationUserCheck.CheckLogType.RELATION_APPLY.getKey())
				.setOperateResult(EnumEnterpriseRelationUserCheck.CheckType.IN_AUDIT.getKey())
				.setEnterpriseId(enterprisesId)
				.setEnterpriseName(enterpriseName)
				.setStatus(EnumCommon.LogicStatus.NORMAL.getKey());
		if(enterprisesRelationUserCheckLogMapper.insert(log)!=1){
			throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_ASSOCIATED_SAVE_FAILED);
		}
	}

	/**
     * 
     * 根据手机号码和姓名申请时间查询企业申请列表
     */
	@Override
	public List<EnterprisesRelationUserCheckDTO> getApplyList(EnterprisesRelationUserCheckRQ rq,Integer enterpriseId,Pagination pagination) {
		Map<String,Object> paramMap = new HashedMap();
		if (!StringUtils.isEmpty(rq.getNameOrPhone())){
			paramMap.put("nameOrPhone", rq.getNameOrPhone());
		}
		paramMap.put("enterpriseId", enterpriseId);
		if (!ObjectUtils.isEmpty(rq.getStartTime())){
			paramMap.put("startTime", rq.getStartTime() + " 00:00:00");
		}
		if (!ObjectUtils.isEmpty(rq.getEndTime())){
			paramMap.put("endTime", rq.getEndTime() + " 23:59:59");
		}
		ArrayList<EnterprisesRelationUserCheck> enterprisesRelationUserChecks = enterprisesRelationUserCheckMapper.getAllApplyList(paramMap,pagination);
		List<EnterprisesRelationUserCheckDTO> list =  BeanUtils.assemble(EnterprisesRelationUserCheckDTO.class, enterprisesRelationUserChecks);
		return list;

	}
	
	/**
	 * 根据ID修改企业关联审核
	 */
	 @Override
	 public ResponseResult<EnterprisesRelationUserCheck> enterprisesRelationUserCheckUpDate(EnterprisesRelationUserCheckAmendRQ rq,AuthPlatformUserInfo userInfo) {
		 EnterprisesRelationUserCheck enterprisesRelationUserCheck = enterprisesRelationUserCheckMapper.selectById(rq.getId());
		 EnterprisesRelationUserCheckLog enterprisesRelationUserCheckLog = new EnterprisesRelationUserCheckLog();
		 String nickname = userInfo.getNickname();
         Integer rqUserId = rq.getUserId();
         Integer userId = userInfo.getId();
		 Date date = new Date();
		 MiddleSystemNoticeRQ middleSystemNoticeRQ = new MiddleSystemNoticeRQ();
		 if (!(2 == enterprisesRelationUserCheck.getCheckStatus())) {
			 return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_USER_REPEATED_AUDIT);
		}
		 //审核拒绝
		 if (0 == rq.getCheckStatus()) {
			 enterprisesRelationUserCheck.setCheckStatus(rq.getCheckStatus())
			 							.setRefusalReason(rq.getRefusalReason())
			 							.setModifyTime(date)
					 					.setCheckId(userId)
			 							.setModifier(nickname);
			 enterprisesRelationUserCheckMapper.updateById(enterprisesRelationUserCheck);
			 BeanUtils.copyProperties(enterprisesRelationUserCheck, enterprisesRelationUserCheckLog);
			 enterprisesRelationUserCheckLog.setEnterpriseRelationUserCheckId(rq.getId())
			 									//0审核申请
			 								.setOperateType(0)
					 						.setCreator(enterprisesRelationUserCheck.getUserName())
                                            .setCreateId(userId)
			 								.setOperateName(nickname)
			 								.setOperateId(userId)
			 								.setOperateTime(date)
			 								.setOperateResult(rq.getCheckStatus());
			 enterprisesRelationUserCheckLogMapper.insert(enterprisesRelationUserCheckLog);
			 //企业关联审批不通过    您提交的关联${enterprise_name}的申请审核已通过，可在企业关联页面查看详情。
			this.sendSmsAfterCheckUpDate(enterprisesRelationUserCheck.getPhone(),
					enterprisesRelationUserCheck.getEnterpriseName(),
					NoticeTemplateType.ENTERPRISE_ASSOCIATION_AUDIT_FAILED.getKey());
	        middleSystemNoticeService.createNotice(userInfo.getId(),EnumMiddleNoticeTitle.title.ENTERPRISE_RELATION_APPLY_RESULT.getValue(),
	                MiddleNoticeContentTemplate.ENTERPRISE_RELATE_APPLY_REFUSE.getKey(),enterprisesRelationUserCheck.getEnterpriseName());
		}
		 //审核通过
		 if (1 == rq.getCheckStatus()) {
			 enterprisesRelationUserCheck.setCheckStatus(rq.getCheckStatus())
			 							.setModifyTime(date)
					 					.setCheckId(userId)
			 							.setModifier(nickname);
			 enterprisesRelationUserCheckMapper.updateById(enterprisesRelationUserCheck);
			 enterprisesRelationUserCheckLog.setEnterpriseRelationUserCheckId(rq.getId())
												//0审核申请
			 								.setOperateType(1)
                                            .setCreateId(userId)
					 						.setCreator(enterprisesRelationUserCheck.getUserName())
			 								.setOperateName(nickname)
			 								.setOperateId(userId)
			 								.setOperateTime(date)
			 								.setOperateResult(rq.getCheckStatus());
			 enterprisesRelationUserCheckLogMapper.insert(enterprisesRelationUserCheckLog);
			 //查询企业部门
			 Departments departments=departmentsService.selectList(new EntityWrapper<Departments>().where("org_id={0}",
					 enterprisesRelationUserCheck.getEnterpriseId())).get(0);
			 // 在企业用户表中关联平台用户ID
			 EnterprisesUsers enterprisesUsers=new EnterprisesUsers().setUserId(enterprisesRelationUserCheck.getUserId())
					 .setIsActive(1)
					 .setIsInvite(1)
					 .setDepartmentsid(departments.getId())
					 .setNickname(enterprisesRelationUserCheck.getUserName())
					 .setEnterpriseId(userInfo.getOrgId())
					 .setRoleId(RoleType.USER.getCode());
			 enterprisesUsersService.insert(enterprisesUsers);
			 UserRole userRole = new UserRole()
					 .setUserId(enterprisesRelationUserCheck.getUserId())
					 .setOrgId(userInfo.getOrgId())
					 //默认是普通用户角色
					 .setRoleId(RoleType.USER.getCode());
			 usersRolesService.insert(userRole);
			 //企业关联审批通过    您提交的关联${enterprise_name}的申请审核已通过，可在企业关联页面查看详情。
			this.sendSmsAfterCheckUpDate(enterprisesRelationUserCheck.getPhone(),
					enterprisesRelationUserCheck.getEnterpriseName(),
					NoticeTemplateType.ENTERPRISE_ASSOCIATION_AUDIT_APPROVED.getKey());
			 middleSystemNoticeService.createNotice(userInfo.getId(),EnumMiddleNoticeTitle.title.ENTERPRISE_RELATION_APPLY_RESULT.getValue(),
					 MiddleNoticeContentTemplate.ENTERPRISE_RELATE_APPLY_PASS.getKey(),enterprisesRelationUserCheck.getEnterpriseName());
		}
		 return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesRelationUserCheck);
		 
	 }
	 
	 /**
	  * 关联审核详情页
	  */
	 @Override
	 public ResponseResult<EnterprisesRelationUserCheckDetailsDTO> getEnterprisesRelationUserCheckDetails(Integer id, AuthPlatformUserInfo userInfo){
		 EnterprisesRelationUserCheck enterprisesRelationUserCheck = enterprisesRelationUserCheckMapper.selectById(id);
		 User user = userMapper.selectById(enterprisesRelationUserCheck.getUserId());
		 Enterprises enterprises = enterprisesMapper.selectById(enterprisesRelationUserCheck.getEnterpriseId());
		 EnterprisesRelationUserCheckDetailsDTO enterprisesRelationUserCheckDetailsDTO = new EnterprisesRelationUserCheckDetailsDTO();
		 ArrayList<Object> list = new ArrayList<>();
		 List<EnterprisesRelationUserCheckLog> selectEnterprisesRelationUserCheckLogs = enterprisesRelationUserCheckLogMapper.selectEnterprisesRelationUserCheckLog(id);
		 list.add(selectEnterprisesRelationUserCheckLogs);
		 enterprisesRelationUserCheckDetailsDTO.setHead(user.getHead())
		 										.setNickname(enterprisesRelationUserCheck.getUserName())
		 										.setEnterpriseAddress(enterprises.getAddress())
		 										.setPhone(enterprisesRelationUserCheck.getPhone())
		 										.setEmail(user.getEmail())
		 										.setFixtel(user.getFixtel())
		 										.setApplyReason(enterprisesRelationUserCheck.getApplyReason())
		 										.setList(list);
		 										
		 return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesRelationUserCheckDetailsDTO);
	 }
	 
	 /**
	  * 模糊查询企业审核列表
	  */
//	 @Override
//	 public ResponseResult<ArrayList<EnterprisesRelationUserCheckDTO>> getAllApplyList(Integer enterpriseId,Pagination pagination){
//		ArrayList<EnterprisesRelationUserCheckDTO> list = enterprisesRelationUserCheckMapper.getAllApplyList(enterpriseId,pagination);
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
//	 }


	/**
	 * 用户关联企业名称校验
	 * @param name 公司名称
	 * @return 是否可关联
	 */
	@Override
	public ResponseResult enterpriseRelationCheck(HttpServletRequest request,String name) {
		// 校验参数是否正确
		if(StringUtils.isEmpty(name)){
			log.error("参数异常!调用{}的{}方法出错 ", "EnterprisesServiceImpl", "enterpriseRelationCheck()");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		UserInfo userInfo = usersService.getSelfInfo(request.getHeader(SYS_TOKEN));
		if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())){
			log.error("获取用户信息异常!调用{}的{}方法出错 ", "EnterprisesServiceImpl", "enterpriseRelationCheck()");
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
		String companyName = name.replaceAll("\\s*", "");
		List<Enterprises> enterprises = enterprisesService.selectList(new EntityWrapper<Enterprises>().eq("name", companyName));
		// 校验企业是否存在
		if(CollectionUtils.isEmpty(enterprises)){
			log.info("企业不存在");
			return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST, EnumEnterpriseCheck.checkRelationCompany.ENTERPRISE_NON_EXIST.getKey());
		}
		Integer userId = userInfo.getId();
		// 企业id
		Integer enterprisesId = enterprises.get(0).getId();
		// 获取用户关联过的企业id集合
		List<Integer> ids = enterprisesUsersService.selectList(new EntityWrapper<EnterprisesUsers>().eq("user_id", userId))
				.stream().map(EnterprisesUsers::getEnterpriseId).collect(Collectors.toList());
		// 校验用户是否已关联该企业
		// 用户已提交过该企业 审核已通过的
		EnterprisesRelationUserCheck one = this.selectOne(new EntityWrapper<EnterprisesRelationUserCheck>()
				.eq("enterprise_name",companyName)
				.eq("user_id",userId)
				.eq("check_status",EnumEnterpriseRelationUserCheck.CheckType.PASSED.getKey())
				.eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
		if(ids.contains(enterprisesId)||!ObjectUtils.isEmpty(one)){
			log.info("用户已关联过该企业");
			return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_ALREADY_ASSOCIATED,EnumEnterpriseCheck.checkRelationCompany.ENTERPRISE_ALREADY_ASSOCIATED.getKey());
		}
		// 用户已提交过该企业 未审核的
		EnterprisesRelationUserCheck two = this.selectOne(new EntityWrapper<EnterprisesRelationUserCheck>()
				.eq("enterprise_name",companyName)
				.eq("user_id",userId)
				.eq("check_status",EnumEnterpriseRelationUserCheck.CheckType.IN_AUDIT.getKey())
				.eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
		if(!ObjectUtils.isEmpty(two)){
			log.info("用户已提交过该企业的关联申请，正在审核中，不可重复提交");
			return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_ASSOCIATED_IN_AUDIT,EnumEnterpriseCheck.checkRelationCompany.ENTERPRISE_ASSOCIATED_IN_AUDIT.getKey());
		}
		// 用户可关联该企业
		log.info("用户可关联该企业");
		return  ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_IS_OK,EnumEnterpriseCheck.checkRelationCompany.ENTERPRISE_IS_OK.getKey());

	}



    /**
     * @Description 企业关联审批通过或者不通过后向申请人发送短信
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    private void sendSmsAfterCheckUpDate(String phone, String enterpriseName,Integer key) {
        try {
            if (!ValidateUtils.isPhone(phone)) {
                log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
            }
            //组装参数
            JSONObject sendJson = new JSONObject();
            sendJson.put("enterprise_name", enterpriseName);
            smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());

            //保存发送消息
			User user = userMapper.selectOne(new User().setUsername(phone));
			Integer noticeTemplateKey;
			if(NoticeTemplateType.ENTERPRISE_ASSOCIATION_AUDIT_APPROVED.getKey() == key){
				noticeTemplateKey = MiddleNoticeContentTemplate.ENTERPRISE_RELATE_APPLY_PASS.getKey();
			}else{
				noticeTemplateKey = MiddleNoticeContentTemplate.ENTERPRISE_RELATE_APPLY_REFUSE.getKey();
			}
			middleSystemNoticeService.createNotice(user.getId(),
					EnumMiddleNoticeTitle.title.ENTERPRISE_RELATION_APPLY_RESULT.getValue(),
					noticeTemplateKey,
					new Object[]{enterpriseName});
        } catch (Exception e) {
            log.info(e.getMessage());
        }
    }
}

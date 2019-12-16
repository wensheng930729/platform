package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumAttacheType;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumEnterpriseLevel;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.CommonUtils;
import com.bee.platform.common.utils.ConstInfos;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dao.mapper.AuthEnterpriseMapper;
import com.bee.platform.user.authority.dao.mapper.AuthPlatformUserMapper;
import com.bee.platform.user.authority.dto.AuthCommonFileDTO;
import com.bee.platform.user.authority.entity.AuthCommonFile;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.rq.EnterpriseRegisterInfoRQ;
import com.bee.platform.user.authority.service.AuthCommonFileService;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck.CheckTypeCommon;
import com.bee.platform.user.constants.enums.EnumRole;
import com.bee.platform.user.dao.mapper.*;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.rq.EnterprisesAttachmentRQ;
import com.bee.platform.user.service.*;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.ValidateUtils;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

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
public class EnterprisesCheckServiceImpl extends ServiceImpl<EnterprisesCheckMapper, EnterprisesCheck> implements EnterprisesCheckService {

    @Autowired
    private EnterprisesCheckLogService enterprisesCheckLogService;
    @Autowired
    private EnterprisesCheckAttachmentService enterprisesCheckAttachmentService;
    @Autowired
    private EnterprisesCheckAttachmentMapper enterprisesCheckAttachmentMapper;
    @Autowired
    private EnterprisesCheckMapper enterprisesCheckMapper;
    @Autowired
    private AuthPlatformUserEnterpriseService userEnterpriseService;
    @Autowired
    private AuthPlatformUserMapper userMapper;
    @Autowired
    private AuthPlatformUserService userSercice;
    @Autowired
    private AuthEnterpriseMapper enterprisesMapper;
    @Autowired
    private AuthEnterpriseService enterprisesService;
    @Autowired
    private UsersRolesMapper usersRolesMapper;
    @Autowired
    private DepartmentsMapper departmentsMapper;
    @Autowired
    private UsersDepartmentsMapper usersDepartmentsMapper;
    @Autowired
    private RegionService regionService;
    @Autowired
    private EnterprisesAppsMapper enterprisesAppsMapper;
    @Autowired
    private EnterprisesCheckAttachmentMapper checkAttachmentMapper;
    @Autowired
    private EnterprisesCheckAttachmentService checkAttachmentService;
    @Autowired
    private CommonRegionService commonRegionService;
    @Autowired
    private SmsService smsService;
    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;
    @Autowired
    private SystemNoticeService systemNoticeService;
    @Autowired
    private ConfigService configService;
    @Autowired
    private MRoleRoleMapper mRoleRoleMapper;
    @Autowired
    private OperatorLogService operatorLogService;
    @Autowired
    private IndustryService industryService;
    @Autowired
    private AuthCommonFileService commonFileService;

    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<String> register(AuthPlatformUserInfo userInfo, EnterpriseRegisterInfoRQ enterpriseRegisterInfoRQ) {
        Integer userId = userInfo.getId();
        String userName = userInfo.getNickname();
        if(userName==null){
            userName=userInfo.getName();
        }
        String phone = userInfo.getPhone();
        Date time = new Date();
        Integer regionId = null;
        try {
            regionId = Integer.parseInt(enterpriseRegisterInfoRQ.getRegionid());
        } catch (NumberFormatException e) {
            log.error("企业申请获取地区id，数字转换异常" + e);
        }
        String street = enterpriseRegisterInfoRQ.getStreet();
        // 设置详细地址
        String address = this.assembleFullAddress(regionId, street);
        enterpriseRegisterInfoRQ.setAddress(address);
        // 申请注册公司名称
        String name = enterpriseRegisterInfoRQ.getName().replaceAll("\\s*", "");
        enterpriseRegisterInfoRQ.setName(name);
        // 查询企业表是否有该企业
        List<AuthEnterprise> one = enterprisesService.selectList(new EntityWrapper<AuthEnterprise>()
                .eq("name", name)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        // 查询企业审核表该企业是否有已通过的记录
        List<Integer> pass = Arrays.asList(1, 3, 4, 5);
        List<EnterprisesCheck> two = this.selectList(new EntityWrapper<EnterprisesCheck>()
                .in("type", pass)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("name", name));
        // 校验企业名称是否已被注册
        if (!CollectionUtils.isEmpty(one) || !CollectionUtils.isEmpty(two)) {
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_EXIST);
        }
        // 查询企业审核表是否有该企业正在审核中的记录
        List<EnterprisesCheck> three = this.selectList(new EntityWrapper<EnterprisesCheck>()
                .eq("type", EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("name", name));
        // 校验该企业名称是否正在审核中
        if (!CollectionUtils.isEmpty(three)) {
            log.info("企业正在审核中");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_IN_AUDIT);
        }
        // 被拒绝的不属于当前用户的企业
        List<EnterprisesCheck> refused = this.selectList(new EntityWrapper<EnterprisesCheck>()
                .eq("name", name)
                .eq("type", EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if (refused.size() > 1) {
            log.info("企业正在审核中");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_IN_AUDIT);

        }
        if (!CollectionUtils.isEmpty(refused) && !refused.get(0).getCreateId().equals(userInfo.getId())) {
            log.info("企业正在审核中");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_IN_AUDIT);
        }

        // 查询企业审核表是否有该企业已申请过被拒绝的记录
        List<EnterprisesCheck> four = this.selectList(new EntityWrapper<EnterprisesCheck>()
                .eq("type", EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("create_id", userId)
                .eq("name", name));
        if (four.size() > 1) {
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_REPEAT);
        }
        // 校验是新申请的企业还是重新提交的企业
        if (CollectionUtils.isEmpty(four)) {
            // 新申请的企业
            EnterprisesCheck enterprisesCheck = BeanUtils.copyProperties(enterpriseRegisterInfoRQ, EnterprisesCheck.class);
            enterprisesCheck.setType(EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey())
                    .setAdmin(phone)
                    .setCreateId(userId)
                    .setCreator(userName)
                    .setCreateAt(time)
                    .setModifyId(userId)
                    .setModifier(userName)
                    .setUpdateAt(time);

            // 写入企业审核表
            if (!insert(enterprisesCheck)) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_CHECK_SAVE_FAILED);
            }
            // 保存企业附件信息
            this.saveEnterprisesCheckAttachment(userInfo, enterpriseRegisterInfoRQ, enterprisesCheck);
            // 写入日志信息
            Integer checkId = enterprisesCheck.getId();
            this.saveCheckLog(name, userId, userName, time, checkId);
        } else {
            // 被拒绝的企业重新修改信息
            Integer id = four.get(0).getId();
            EnterprisesCheck enterprisesCheck = BeanUtils.copyProperties(enterpriseRegisterInfoRQ, four.get(0));
            enterprisesCheck.setId(id)
                    .setType(EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey())
                    .setAdmin(phone)
                    .setCreateId(userId)
                    .setCreator(userName)
                    .setModifyId(userId)
                    .setModifier(userName)
                    .setUpdateAt(time);
            // 更新企业审核表信息
            if (!updateById(enterprisesCheck)) {
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ENTERPRISE_CHECK_UPDATE_FAILED);
            }
            // 逻辑删除企业附件信息包括(0营业执照 1营业许可证 2企业认证授权书)
            if (!enterprisesCheckAttachmentService.update(new EnterprisesCheckAttachment().setStatus(EnumCommon.LogicStatus.DELETED.getKey()),
                    new EntityWrapper<EnterprisesCheckAttachment>()
                            .eq("enterprises_check_id", id)
                            .ne("type", EnumAttacheType.type.logo.getKey()))) {
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.ENTERPRISE_CHECK_UPDATE_FAILED);
            }
            // 重新写入新的企业附件信息
            this.saveEnterprisesCheckAttachment(userInfo, enterpriseRegisterInfoRQ, enterprisesCheck);
            // 保存操作日志信息
            Integer checkId = enterprisesCheck.getId();
            this.saveCheckLog(name, userId, userName, time, checkId);
        }
        // 保存企业申请中台系统通知消息
        String title = EnumMiddleNoticeTitle.title.ENTERPRISE_APPLY.getValue();
        middleSystemNoticeService.createNotice(userId, title, MiddleNoticeContentTemplate.ENTERPRISE_APPLY.getKey(), name);
        // 保存企业申请后台系统通知消息
        Config config = configService.getConfigByconfigKey("enterprisesCheck_audit_roleId");
        if (!ObjectUtils.isEmpty(config)) {
            String configValue = config.getConfigValue();
            Integer childId = null;
            try {
                childId = Integer.parseInt(configValue);
            } catch (NumberFormatException e) {
                log.error("企业申请生成后台系统通知，获取ManagerIds，数字转换异常" + e);
            }
            List<Integer> mIds = mRoleRoleMapper.getManagerIdsByChildId(childId);
            int[] array1 = mIds.stream().mapToInt(Integer::valueOf).toArray();
            List<SystemNotice> notice = systemNoticeService.createNotice(array1, NoticeTemplateType.ENTERPRISE_APPLY, name);
            systemNoticeService.insertAll(notice);
        }

        return ResponseResult.success("提交审核成功！请耐心等待审批");

    }

    public String assembleFullAddress(Integer districtId, String street) {
        ResponseResult<Map<String, Object>> ret = commonRegionService.findAllRegionById(districtId);
        Map<String, Object> returnMap = (Map<String, Object>) ret.getObject();
        if (returnMap == null) {
            log.info("获取地址信息失败, districtId: {}", districtId);
            return null;
        }
        CommonRegion province = (CommonRegion) returnMap.get("province");
        CommonRegion city = (CommonRegion) returnMap.get("city");
        CommonRegion county = (CommonRegion) returnMap.get("county");
        StringBuilder sb = new StringBuilder();
        sb.append(province.getDistrict());
        sb.append(" " + city.getDistrict());
        sb.append(" " + county.getDistrict());
        sb.append(" " + street);
        return sb.toString();
    }

    @Override
    public ResponseResult<EnterpriseWithAttacheDTO> getCompanyInfo(AuthPlatformUserInfo userInfo) {
        Integer orgId = userInfo.getOrgId();
        // 公司信息
        AuthEnterprise enterprise = enterprisesMapper.selectOne(
                new AuthEnterprise().setId(orgId).setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        EnterpriseWithAttacheDTO result = new EnterpriseWithAttacheDTO();
        BeanUtils.copyProperties(enterprise, result);
        // 附件信息
        List<AuthCommonFile> attachments = commonFileService.selectList(new EntityWrapper<AuthCommonFile>()
                .eq("enterprises_id", orgId)
                .eq("status", EnumCommon.IsActive.is_active.getKey()));
        if (!CollectionUtils.isEmpty(attachments)) {
            // 执照
            List<AuthCommonFile> enclosures = attachments.stream()
                    .filter(a -> a.getType().equals(EnumAttacheType.type.enclosure.getKey()))
                    .collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(enclosures)) {
                List<AuthCommonFileDTO> enclosuresList = BeanUtils.assemble(AuthCommonFileDTO.class, enclosures);
                result.setEnclosuresList(enclosuresList);
            }
            // 许可证
            List<AuthCommonFile> permits = attachments.stream()
                    .filter(a -> a.getType().equals(EnumAttacheType.type.permit.getKey()))
                    .collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(permits)) {
                List<AuthCommonFileDTO> permitsList = BeanUtils.assemble(AuthCommonFileDTO.class, permits);
                result.setPermitsList(permitsList);
            }
            // 授权书
            List<AuthCommonFile> certificates = attachments.stream()
                    .filter(a -> a.getType().equals(EnumAttacheType.type.certificate.getKey()))
                    .collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(certificates)) {
                List<AuthCommonFileDTO> certificatesList = BeanUtils.assemble(AuthCommonFileDTO.class,
                        certificates);
                result.setCertificatesList(certificatesList);
            }
            // logo
            List<AuthCommonFile> logos = attachments.stream()
                    .filter(a -> a.getType().equals(EnumAttacheType.type.logo.getKey()))
                    .collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(logos)) {
                List<AuthCommonFileDTO> logosList = BeanUtils.assemble(AuthCommonFileDTO.class, logos);
                result.setLogosList(logosList);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> updateCompanyInfo(AuthPlatformUserInfo userInfo, EnterpriseRegisterInfoRQ rq) {
        EnterprisesCheck originCheck = enterprisesCheckMapper
                .selectOne(new EnterprisesCheck().setRealId(rq.getId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(originCheck)) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_CHECK_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
        }
        // 查询公司名称是否已经被注册(审核中和审核通过即已注册)
        List<Integer> types = new ArrayList<>();
        types.add(EnumEnterpriseCheck.CheckType.PASSED_REGISTER.getKey());
        types.add(EnumEnterpriseCheck.CheckType.PASSED_MODIFY.getKey());
        types.add(EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey());
        types.add(EnumEnterpriseCheck.CheckType.IN_AUDIT_MODIFY.getKey());
        Integer enterpriseId = rq.getId();
        String name = rq.getName();
        //排除保留原有企业名修改
        AuthEnterprise enterprise = enterprisesMapper.selectOne(
                new AuthEnterprise().setId(enterpriseId).setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        EnterprisesCheck check = enterprisesCheckMapper.selectOne(
                new EnterprisesCheck().setStatus(EnumCommon.LogicStatus.NORMAL.getKey()).setRealId(enterpriseId));
        List<EnterprisesCheck> enterprisesCheck = enterprisesCheckMapper.selectList(
                new EntityWrapper<EnterprisesCheck>()
                        .in("type", types).eq("name", name)
                        .eq("status", EnumCommon.IsActive.is_active.getKey()));
        if (!CollectionUtils.isEmpty(enterprisesCheck) && !enterprise.getName().equals(name) && !check.getName().equals(name)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NAME_EXISTED);
        }
        EnterprisesCheck enterpriseCheck = new EnterprisesCheck();
        BeanUtils.copyProperties(rq, enterpriseCheck);
        //check表id
        Integer checkId = originCheck.getId();
        enterpriseCheck.setId(checkId);
        enterpriseCheck.setType(EnumEnterpriseCheck.CheckType.IN_AUDIT_MODIFY.getKey());
        enterpriseCheck.setUpdateAt(new Date());
        // 设置详细地址
        Integer regionId = Integer.parseInt(rq.getRegionid());
        String street = rq.getStreet();
        String address = this.assembleFullAddress(regionId, street);
        enterpriseCheck.setAddress(address);
        // 修改公司check表和附件表
        List<EnterprisesCheckAttachment> checkAttachments = checkAttachmentMapper
                .selectList(new EntityWrapper<EnterprisesCheckAttachment>().eq("enterprises_check_id", checkId));
        if (!CollectionUtils.isEmpty(checkAttachments)) {
            // 删除所有已有附件
            checkAttachments.stream().forEach(e -> e.setStatus(EnumCommon.IsActive.not_active.getKey()));
            if (!checkAttachmentService.updateAllColumnBatchById(checkAttachments)) {
                throw new BusinessException(ResCodeEnum.FAILED, ExceptionMessageEnum.ENTERPRISE_ATTACHE_UPDATE_FAILED);
            }
        }
        // 保存企业信息
        saveEnterprisesCheckAttachment(userInfo, rq, enterpriseCheck);
        if (enterprisesCheckMapper.updateById(enterpriseCheck) <= 0) {
            throw new BusinessException(ResCodeEnum.FAILED, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
        }
        // 中台系统通知
        String checkName = enterpriseCheck.getName();
        String title = EnumMiddleNoticeTitle.title.ENTERPRISE_CHANGE_DATA_NOTICE.getValue();
        middleSystemNoticeService.createNotice(userInfo.getId(), title,
                MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_UPDATE_ENTERPRISES_INFO.getKey(), checkName, new Date());
        // 后台系统消息通知人
        String childRoleId = configService.getConfigByconfigKey("enterprisesCheck_audit_roleId").getConfigValue();
        List<Integer> managers = mRoleRoleMapper.getManagerIdsByChildId(Integer.valueOf(childRoleId));
        int[] managerIds = managers.stream().mapToInt(Integer::valueOf).toArray();

        List<SystemNotice> notices = systemNoticeService.createNotice(managerIds,
                NoticeTemplateType.ENTERPRISE_INFO_UPDATE, originCheck.getName());
        // 插入系统通知
        if (!systemNoticeService.insertBatch(notices)) {
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
        }
        //发送短信
        try {
            sendSmsAfterEnterprisesCheck(enterpriseCheck.getAdmin(), enterpriseCheck.getName(),
                    NoticeTemplateType.ENTERPRISE_INFO_UPDATE.getKey());
        } catch (Exception e) {
            log.error("手机提示后台管理员用户状态变更消息发送失败，手机号是:{}。错误信息是:{}", userInfo.getUsername(), e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 保存企业附件
     *
     * @param userInfo                 用户信息
     * @param enterpriseRegisterInfoRQ 企业申请参数
     * @param enterprisesCheck         企业审核记录信息
     */
    private void saveEnterprisesCheckAttachment(AuthPlatformUserInfo userInfo, EnterpriseRegisterInfoRQ enterpriseRegisterInfoRQ,
                                                EnterprisesCheck enterprisesCheck) {
        // 校验参数
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(enterpriseRegisterInfoRQ)
                || ObjectUtils.isEmpty(enterprisesCheck)) {
            throw new BusinessException(ResCodeEnum.FAILED, ExceptionMessageEnum.ENTERPRISE_CHECK_SAVE_FAILED);
        }
        // 组装附件列表
        List<EnterprisesAttachmentRQ> attachment = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(enterpriseRegisterInfoRQ.getEnclosuresList())) {
            attachment.addAll(enterpriseRegisterInfoRQ.getEnclosuresList());
        }
        if (!CollectionUtils.isEmpty(enterpriseRegisterInfoRQ.getCertificatesList())) {
            attachment.addAll(enterpriseRegisterInfoRQ.getCertificatesList());
        }
        if (!CollectionUtils.isEmpty(enterpriseRegisterInfoRQ.getPermitsList())) {
            attachment.addAll(enterpriseRegisterInfoRQ.getPermitsList());
        }
        if (!CollectionUtils.isEmpty(enterpriseRegisterInfoRQ.getLogosList())) {
            attachment.addAll(enterpriseRegisterInfoRQ.getLogosList());
        }
        // 设置附件关联企业审核表id
        List<EnterprisesAttachmentRQ> attachmentRQS = attachment.stream()
                .map(obj -> obj.setEnterprisesCheckId(enterprisesCheck.getId())).collect(Collectors.toList());
        // 写入企业附件信息
        if (!enterprisesCheckAttachmentService.saveEnterprisesCheckAttachment(userInfo, attachmentRQS)) {
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_CHECK_SAVE_FAILED);
        }
    }

    /**
     * 保存审核日志信息
     *
     * @param name     公司名称
     * @param userId   用户id
     * @param userName 用户名称
     * @param time     时间
     * @param checkId  审核id
     */
    private void saveCheckLog(String name, Integer userId, String userName, Date time, Integer checkId) {

        // 写入日志信息
        EnterprisesCheckLog enterprisesCheckLog = new EnterprisesCheckLog().setEnterpriseCheckId(checkId)
                .setEnterpriseName(name).setCreateId(userId).setCreator(userName).setCreateTime(time)
                .setModifyId(userId).setModifier(userName).setModifyTime(time)
                .setOperateType(EnumEnterpriseCheck.checkLogType.ADMISSION_APPLY.getKey()).setOperateId(userId)
                .setOperateName(userName).setOperateTime(time)
                .setOperateResult(EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey())
                .setStatus(EnumEnterpriseCheck.checkLogStatus.NORMAL.getKey());
        if (!enterprisesCheckLogService.insert(enterprisesCheckLog)) {
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_CHECK_SAVE_FAILED);
        }
    }

    /**
     * 查询用户申请认证还未通过的企业信息
     *
     * @param phone
     * @return
     */
    @Override
    public List<EnterprisesCheck> getAuthenticatedList(String phone) {
        // 检索未通过、新建、更新的认证单
        List<Integer> types = new ArrayList<>();
        types.add(ConstInfos.REVIEW.NO.ordinal());
        types.add(ConstInfos.REVIEW.UPDATE.ordinal());
        types.add(ConstInfos.REVIEW.NEW.ordinal());
        List<EnterprisesCheck> enterpriseChecks = enterprisesCheckMapper.findByTypes(types, phone);
        return enterpriseChecks;
    }

//	/**
//	 * 通过企业审核表id撤回认证记录
//	 *
//	 * @param checkId
//	 * @return
//	 */
//	@Override
//	public ResponseResult<String> revokeAuthentication(Integer checkId) {
//		// 根据id查询认证单
//		EnterprisesCheck enterpriseCheck = enterprisesCheckMapper.selectById(checkId);
//
//		if (ObjectUtils.isEmpty(enterpriseCheck)) {
//			throw new BusinessException(ResCodeEnum.ENTERPRISE_CHECK_NOT_EXIST,
//					ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
//		}
//		// 判断认证单是否处于审核未通过状态
//		if (ConstInfos.REVIEW.NEW.ordinal() != enterpriseCheck.getType()
//				&& ConstInfos.REVIEW.UPDATE.ordinal() != enterpriseCheck.getType()
//				&& ConstInfos.REVIEW.NO.ordinal() != enterpriseCheck.getType()) {
//			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//		}
//		enterpriseCheck.setType(ConstInfos.REVIEW.REVOKE.ordinal());
//		enterpriseCheck.setUpdateAt(new Timestamp(System.currentTimeMillis()));
//		// 修改状态
//		if (enterprisesCheckMapper.updateById(enterpriseCheck) <= Status.FALSE.getKey()) {
//			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_CHECK_UPDATE_FAILED);
//		}
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//	}

//	/**
//	 * 通过企业审核表id再次认证
//	 *
//	 * @param checkId`
//	 * @return
//	 */
//	@Override
//	public ResponseResult<String> reAuthentication(Integer checkId) {
//		// 根据id查询认证单
//		EnterprisesCheck enterpriseCheck = enterprisesCheckMapper.selectById(checkId);
//
//		if (ObjectUtils.isEmpty(enterpriseCheck)) {
//			throw new BusinessException(ResCodeEnum.ENTERPRISE_CHECK_NOT_EXIST,
//					ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
//		}
//
//		// 判断认证单是否处于审核未通过状态
//		if (ConstInfos.REVIEW.NO.ordinal() != enterpriseCheck.getType()) {
//			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//		}
//		enterpriseCheck.setType(ConstInfos.REVIEW.NEW.ordinal());
//		enterpriseCheck.setUpdateAt(new Timestamp(System.currentTimeMillis()));
//		// 修改状态
//		if (enterprisesCheckMapper.updateById(enterpriseCheck) <= Status.FALSE.getKey()) {
//			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_CHECK_UPDATE_FAILED);
//		}
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//	}

    /**
     * 通过企业审核表id查询企业审核详细信息
     *
     * @param checkId
     * @return
     */
    @Override
    public EnterpriseCheckDTO getEnterpriseCheckInfo(Integer checkId) {
        // 根据id查询认证单
        EnterprisesCheck enterprisesCheck = enterprisesCheckMapper.selectById(checkId);
        if (ObjectUtils.isEmpty(enterprisesCheck)) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_CHECK_NOT_EXIST,
                    ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
        }
        EnterpriseCheckDTO enterpriseCheckDTO = BeanUtils.copyProperties(enterprisesCheck, EnterpriseCheckDTO.class);
        // 获取地区
        if (!"".equals(enterpriseCheckDTO.getRegionid()) && enterpriseCheckDTO.getRegionid() != null) {
            try {
                // 获取到区级信息
                RegionInfo county = regionService.findRegionById(Integer.valueOf(enterpriseCheckDTO.getRegionid()));
                // 获取到市级信息
                RegionInfo city = regionService.findRegionById(county.getPid());
                // 获取到省级信息
                RegionInfo province = regionService.findRegionById(city.getPid());
                enterpriseCheckDTO.setCounty(county);
                enterpriseCheckDTO.setCity(city);
                enterpriseCheckDTO.setProvince(province);
            } catch (Exception e) {
                // 查询地址错误。捕获异常不做处理，返回一个空对象
            }
        }
        return enterpriseCheckDTO;
    }

    // 先注释，待测试通过之后删除
//	/**
//	 * 审核企业
//	 *
//	 * @param id
//	 * @param type
//	 * @param reason
//	 * @param check_username
//	 * @return
//	 */
//	@Override
//	@Transactional(rollbackFor = Exception.class)
//	public ResponseResult<String> review(int id, int type, String reason, String check_username) {
//
//		try {
//			EnterprisesCheck enterprisesCheck = enterprisesCheckMapper.selectById(id);
//			if (ObjectUtils.isEmpty(enterprisesCheck)) {
//				throw new BusinessException(ResCodeEnum.ENTERPRISE_CHECK_NOT_EXIST,
//						ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
//			}
//			Integer preType = enterprisesCheck.getType();
//			enterprisesCheck.setType(type);
//			// 审核失败，添加审核失败原因
//			if (type == ConstInfos.REVIEW.NO.ordinal()) {
//				enterprisesCheck.setFailureReason(reason);
//			}
//			enterprisesCheck.setCheckId(platformManagersMapper
//					.selectOne(new PlatformManagers().setUsername(check_username)).getManagerId());
//			enterprisesCheck.setUpdateAt(new Timestamp(System.currentTimeMillis()));
//			enterprisesCheckMapper.updateById(enterprisesCheck);
//
//			if (type == ConstInfos.REVIEW.OK.ordinal()) {
//				if (EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey().equals(preType)) {
//					Enterprises enterprise = new Enterprises();
//					enterprise.setLicence(enterprisesCheck.getLicence());
//					enterprise.setEnclosure(enterprisesCheck.getEnclosure());
//					enterprise.setContact(enterprisesCheck.getContact());
//					enterprise.setAddress(enterprisesCheck.getAddress());
//					enterprise.setName(enterprisesCheck.getName());
//					enterprise.setCreateAt(enterprisesCheck.getCreateAt());
//					enterprise.setUpdateAt(enterprisesCheck.getUpdateAt());
//					// 企业类型
//					enterprise.setType(enterprisesCheck.getEnterprisesType());
//					// 企业行业
//					enterprise.setIndustry(enterprisesCheck.getIndustry());
//					// 企业信息
//					if (enterprisesMapper.insert(enterprise) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错，企业信息保存异常", "review", "insert()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
//					}
//					// 企业审核信息
//					// enterprisesCheck.setRealId(enterprisesMapper.selectOne(new
//					// Enterprises().setName(enterprise.getName())).getId());
//					enterprisesCheck.setRealId(enterprise.getId());
//					if (enterprisesCheckMapper.updateById(enterprisesCheck) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错，企业审核信息保存异常", "review", "updateById()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL,
//								ExceptionMessageEnum.ENTERPRISE_CHECK_UPDATE_FAILED);
//					}
//					// 用户角色信息
//					User user = userMapper.selectOne(new User().setPhone(enterprisesCheck.getAdmin()));
//					UserRole userRole = new UserRole();
//					userRole.setUserId(user.getId());
//					userRole.setRoleId(EnumRole.RoleType.superAdmin.getKey());
//					userRole.setOrgId(
//							enterprisesMapper.selectOne(new Enterprises().setName(enterprise.getName())).getId());
//					if (usersRolesMapper.insert(userRole) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错，用户角色关系信息保存异常", "review", "save()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.USER_ROLR_SAVE_FAILED);
//					}
//					// 用户企业信息
//					EnterprisesUsers enterprisesUsers = new EnterprisesUsers();
//					enterprisesUsers.setUserId(user.getId());
//					enterprisesUsers.setEnterpriseId(
//							enterprisesMapper.selectOne(new Enterprises().setName(enterprise.getName())).getId());
//					if (StringUtils.isEmpty(user.getNickname())) {
//						log.info(user.getPhone() + "的用户名为空，设为手机号");
//						user.setNickname(user.getPhone());
//					}
//					enterprisesUsers.setNickname(user.getNickname());
//					enterprisesUsers.setPost("超级管理员");
//					enterprisesUsers.setIsActive(EnumEnterpriseUser.ActiveType.is_active.getKey());
//					enterprisesUsers.setIsInvite(EnumEnterpriseUser.InviteType.is_invite.getKey());
//					enterprisesUsers.setNicknamePinyin(PinyinUtil.toPinyin(user.getNickname()));
//					if (enterprisesUsersMapper.insert(enterprisesUsers) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错，企业用户关系信息保存异常", "review", "insert()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL,
//								ExceptionMessageEnum.USER_ENTERPRISE_RELATION_SAVE_FAILED);
//					}
//					/** 创建企业的同时创建一个同名的部门在该企业下 */
//					Departments department = new Departments();
//					department.setName(enterprisesCheck.getName());
//					int org_id = enterprisesMapper.selectOne(new Enterprises().setName(enterprisesCheck.getName()))
//							.getId();
//					department.setOrgId(org_id);
//					department.setCreateAt(new Timestamp(System.currentTimeMillis()));
//					department.setUpdateAt(new Timestamp(System.currentTimeMillis()));
//
//					if (departmentsMapper.insert(department) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错， 部门信息保存异常", "review", "insert()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
//					}
//					// 部门tree_id
//					department.setTreeId(department.getId());
//					if (departmentsMapper.insert(department) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错， 部门信息tree_id保存异常", "review", "insert()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
//					}
//					/** 将管理员添加到该部门下 */
//					UsersDepartments usersDepartments = new UsersDepartments();
//					usersDepartments.setDepartmentId(departmentsMapper.selectList(new EntityWrapper<Departments>()
//							.eq("org_id", org_id).and().eq("tree_id", department.getTreeId())).get(0).getId());
//					usersDepartments.setUserId(user.getId());
//					if (usersDepartmentsMapper.insert(usersDepartments) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错，用户部门关系信息保存异常", "review", "save()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL,
//								ExceptionMessageEnum.USER_DEPARTMENT_RELATION_SAVE_FAILED);
//					}
//					// 注意此处改版
////                }else if (EnumEnterpriseCheck.CheckType.updated.getKey().equals(preType)) {
//				} else if (EnumEnterpriseCheck.CheckType.IN_AUDIT_MODIFY.getKey().equals(preType)) {
//					EnterprisesCheck check = enterprisesCheckMapper.selectById(id);
//					Enterprises enterprise = enterprisesMapper.selectById(check.getRealId());
//					String org_name = enterprise.getName();
//					enterprise.setLicence(enterprisesCheck.getLicence());
//					enterprise.setEnclosure(enterprisesCheck.getEnclosure());
//					enterprise.setContact(enterprisesCheck.getContact());
//					enterprise.setAddress(enterprisesCheck.getAddress());
//					enterprise.setName(enterprisesCheck.getName());
//					enterprise.setUpdateAt(enterprisesCheck.getUpdateAt());
//					// 企业类型
//					enterprise.setType(enterprisesCheck.getEnterprisesType());
//					// 企业行业
//					enterprise.setIndustry(enterprisesCheck.getIndustry());
//					// 企业信息
//					if (enterprisesMapper.insert(enterprise) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错，企业信息保存异常", "review", "insert()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
//					}
//					// 企业审核信息
//					enterprisesCheck.setRealId(enterprise.getId());
//					if (enterprisesCheckMapper.updateById(enterprisesCheck) <= Status.FALSE.getKey()) {
//						log.error("调用{}的{}方法出错，企业审核信息保存异常", "review", "updateById()");
//						throw new BusinessException(ResCodeEnum.SAVE_FAIL,
//								ExceptionMessageEnum.ENTERPRISE_CHECK_UPDATE_FAILED);
//					}
//					List<Departments> departments = departmentsMapper.selectList(new EntityWrapper<Departments>()
//							.eq("org_id", enterprise.getId()).and().eq("name", org_name));
//					if (!org.apache.commons.collections.CollectionUtils.isEmpty(departments)) {
//						departments.get(0).setName(enterprisesCheck.getName());
//						if (departmentsMapper.updateById(departments.get(0)) <= Status.FALSE.getKey()) {
//							throw new BusinessException(ResCodeEnum.SAVE_FAIL,
//									ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
//						}
//					}
//
//				}
//			}
//		} catch (Exception e) {
//			log.error("调用{}的{}方法出错，企业审核信息保存异常{}", "review", "updateById()", e);
//			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_AUDIT_FAILED);
//		}
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//	}

//	/**
//	 * 企业修改审核
//	 *
//	 * @param id
//	 * @param type
//	 * @param check_username
//	 * @return
//	 */
//	@Override
//	@Transactional(rollbackFor = Exception.class)
//	public ResponseResult<String> reviewUpdate(int id, int type, String check_username) {
//		if (enterprisesCheckMapper.selectCount(new EntityWrapper<EnterprisesCheck>().eq("id", id)) <= 0) {
//			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
//		}
//		try {
//			EnterprisesCheck enterprisesCheck = enterprisesCheckMapper.selectOne(new EnterprisesCheck().setId(id));
//			enterprisesCheck.setType(type);
//			enterprisesCheck.setCheckId(platformManagersMapper
//					.selectOne(new PlatformManagers().setUsername(check_username)).getManagerId());
//			enterprisesCheck.setUpdateAt(new Timestamp(System.currentTimeMillis()));
//			// 修改审核
//			if (enterprisesCheckMapper.updateById(enterprisesCheck) <= Status.FALSE.getKey()) {
//				log.error("调用{}的{}方法出错，企业审核信息保存异常", "reviewUpdate", "updateById()");
//				throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_UPDATE_AUDIT_FAILED);
//			}
//			if (type == ConstInfos.REVIEW.OK.ordinal()) {
//				EnterprisesCheck check = enterprisesCheckMapper.selectById(id);
//				Enterprises enterprise = enterprisesMapper.selectById(check.getRealId());
//				enterprise.setLicence(enterprisesCheck.getLicence());
//				enterprise.setEnclosure(enterprisesCheck.getEnclosure());
//				enterprise.setContact(enterprisesCheck.getContact());
//				enterprise.setAddress(enterprisesCheck.getAddress());
//				enterprise.setName(enterprisesCheck.getName());
//				enterprise.setUpdateAt(enterprisesCheck.getUpdateAt());
//				// 修改审核
//				if (enterprisesMapper.updateById(enterprise) <= Status.FALSE.getKey()) {
//					log.error("调用{}的{}方法出错，企业信息保存异常", "reviewUpdate", "updateById()");
//					throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
//				}
//				enterprisesCheck.setRealId(enterprise.getId());
//				// 修改审核
//				if (enterprisesCheckMapper.updateById(enterprisesCheck) <= Status.FALSE.getKey()) {
//					log.error("调用{}的{}方法出错，企业审核信息保存异常", "reviewUpdate", "updateById()");
//					throw new BusinessException(ResCodeEnum.SAVE_FAIL,
//							ExceptionMessageEnum.ENTERPRISE_UPDATE_AUDIT_FAILED);
//				}
//			}
//		} catch (Exception e) {
//			log.error("调用{}方法出错，企业审核信息保存异常", "reviewUpdate");
//			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_UPDATE_AUDIT_FAILED);
//		}
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//	}

//	/**
//	 * 获取企业审核列表
//	 *
//	 * @return
//	 */
//	@Override
//	public List<EnterprisesCheck> getAllEnterpriseCheck() {
//		return enterprisesCheckMapper.selectList(new EntityWrapper<EnterprisesCheck>());
//	}

    /**
     * 分页获取企业审核列表
     *
     * @param pagination
     * @return
     */
    @Override
    public ResponseResult<Map<String, Object>> getAllEnterpriseCheckByPage(Pagination pagination) {
        List<EnterprisesCheck> enterprisesChecks = enterprisesCheckMapper.getAllEnterpriseCheckByPage(pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                CommonUtils.convertToData(enterprisesChecks, pagination));
    }

//	/**
//	 * 获取未审核企业列表
//	 *
//	 * @return
//	 */
//	@Override
//	public List<EnterprisesCheck> getUntreatedEnterpriseCheck() {
//		List<EnterprisesCheck> enterprisesCheckList = enterprisesCheckMapper.selectList(new EntityWrapper<>());
//		List<EnterprisesCheck> newList = new ArrayList<>();
//		for (EnterprisesCheck check : enterprisesCheckList) {
//			if (check.getType() == ConstInfos.REVIEW.NEW.ordinal()
//					|| check.getType() == ConstInfos.REVIEW.UPDATE.ordinal()) {
//				newList.add(check);
//			}
//		}
//		return newList;
//	}

//	/**
//	 * 获取已审核企业列表
//	 *
//	 * @return
//	 */
//	@Override
//	public List<EnterprisesCheck> getTreatedEnterpriseCheck() {
//		List<EnterprisesCheck> enterprisesCheckList = enterprisesCheckMapper
//				.selectList(new EntityWrapper<EnterprisesCheck>());
//		List<EnterprisesCheck> newList = new ArrayList<>();
//		for (EnterprisesCheck check : enterprisesCheckList) {
//			if (check.getType() == ConstInfos.REVIEW.NO.ordinal()
//					|| check.getType() == ConstInfos.REVIEW.OK.ordinal()) {
//				newList.add(check);
//			}
//		}
//		return newList;
//	}

//	/**
//	 * 获取未处理的企业数量（我的待办数量）、已通过企业数量（已通过企业）、申请企业总数（申请总数）
//	 *
//	 * @return
//	 */
//	@Override
//	public EnterpriseCheckStatusDTO getCounts() {
//
//		EnterpriseCheckStatusDTO enterpriseCheckStatusDTO = new EnterpriseCheckStatusDTO();
//		// 获取待办数量
//		List<Integer> untreatedTypes = new ArrayList<>();
//		untreatedTypes.add(ConstInfos.REVIEW.NEW.ordinal());
//		untreatedTypes.add(ConstInfos.REVIEW.UPDATE.ordinal());
//		Long untreatedCount = enterprisesCheckMapper.getCounts(untreatedTypes);
//		// 获取已通过企业数量
//		List<Integer> passedTypes = new ArrayList<>();
//		passedTypes.add(ConstInfos.REVIEW.OK.ordinal());
//		Long passedCount = enterprisesCheckMapper.getCounts(passedTypes);
//		// 获取申请企业总数
//		List<Integer> SumTypes = new ArrayList<>();
//		SumTypes.add(ConstInfos.REVIEW.NO.ordinal());
//		SumTypes.add(ConstInfos.REVIEW.OK.ordinal());
//		SumTypes.add(ConstInfos.REVIEW.NEW.ordinal());
//		SumTypes.add(ConstInfos.REVIEW.UPDATE.ordinal());
//		Long sumCount = enterprisesCheckMapper.getCounts(SumTypes);
//
//		enterpriseCheckStatusDTO.setUntreatedCount(untreatedCount.intValue());
//		enterpriseCheckStatusDTO.setPassedCount(passedCount.intValue());
//		enterpriseCheckStatusDTO.setSumCount(sumCount.intValue());
//		return enterpriseCheckStatusDTO;
//	}

//	/**
//	 * 分页获取未审核企业列表
//	 *
//	 * @param companyName
//	 * @param pagination
//	 * @return
//	 */
//	@Override
//	public ResponseResult<Map<String, Object>> getUntreatedEnterpriseCheck(String companyName, Pagination pagination) {
//		List<EnterprisesCheck> enterprisesChecks = enterprisesCheckMapper.selectPage(pagination,
//				new EntityWrapper<EnterprisesCheck>().like("name", companyName, SqlLike.DEFAULT)
//						.and("(type={0} or type={1})", 2, 3).orderBy("create_at", false));
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
//				CommonUtils.convertToData(enterprisesChecks, pagination));
//	}

//	/**
//	 * 分页获取已审核企业列表
//	 *
//	 * @param companyName
//	 * @param pagination
//	 * @return
//	 */
//	@Override
//	public ResponseResult<Map<String, Object>> getOkEnterpriseCheck(String companyName, Pagination pagination) {
//		List<EnterprisesCheck> enterprisesChecks = enterprisesCheckMapper.selectPage(pagination,
//				new EntityWrapper<EnterprisesCheck>().like("name", companyName, SqlLike.DEFAULT).eq("type", 1)
//						.orderBy("create_at", false));
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
//				CommonUtils.convertToData(enterprisesChecks, pagination));
//	}

//	/**
//	 * 获取审核未通过企业审核列表（带分页）
//	 *
//	 * @param companyName
//	 * @param pagination
//	 * @return
//	 */
//	@Override
//	public ResponseResult<Map<String, Object>> getNoEnterpriseCheck(String companyName, Pagination pagination) {
//		List<EnterprisesCheck> enterprisesChecks = enterprisesCheckMapper.selectPage(pagination,
//				new EntityWrapper<EnterprisesCheck>().like("name", companyName, SqlLike.DEFAULT).eq("type", 0)
//						.orderBy("create_at", false));
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
//				CommonUtils.convertToData(enterprisesChecks, pagination));
//	}

    /**
     * 根据id查看详情
     *
     * @param enterpriseId
     * @return
     */
    @Override
    public EnterprisesCheck getEnterpriseAllInfo(Integer enterpriseId) {
        return enterprisesCheckMapper.selectOne(new EnterprisesCheck().setId(enterpriseId));
    }

//	/**
//	 * 保存修改后的企业信息
//	 *
//	 * @param enterpriseInfoRQ
//	 * @return
//	 */
//	@Override
//	public ResponseResult<String> updateEnterpriseInfo(EnterpriseInfoRQ enterpriseInfoRQ) {
//		EnterprisesCheck enterprisesCheck = enterprisesCheckMapper
//				.selectOne(new EnterprisesCheck().setId(enterpriseInfoRQ.getId()));
//		enterprisesCheck.setName(enterpriseInfoRQ.getName());
//		enterprisesCheck.setContact(enterpriseInfoRQ.getContact());
//		enterprisesCheck.setLinkman(enterpriseInfoRQ.getLinkman());
//		enterprisesCheck.setAddress(enterpriseInfoRQ.getAddress());
//		enterprisesCheck.setEnclosure(enterpriseInfoRQ.getEnclosure());
//		enterprisesCheck.setPermit(enterpriseInfoRQ.getPermit());
//		enterprisesCheck.setCertificate(enterpriseInfoRQ.getCertificate());
//
//		if (enterprisesCheckMapper.updateById(enterprisesCheck) <= Status.FALSE.getKey()) {
//			log.error("调用{}方法出错，保存修改后的企业信息异常", "updateEnterpriseInfo");
//			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
//		}
//		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//	}

    @Override
    public EnterpriseCheckDetailDTO getCheckInfo(Integer id) {
        EnterpriseCheckDetailDTO dto = new EnterpriseCheckDetailDTO();
        EnterprisesCheck enterprisesCheck = enterprisesCheckMapper.selectById(id);
        BeanUtils.copyProperties(enterprisesCheck, dto);
        // 行业信息
        if (StringUtils.isEmpty(enterprisesCheck.getIndustry())) {
            dto.setIndustry(null);
        } else {
            dto.setIndustry(industryService.getIndustryById(Integer.parseInt(enterprisesCheck.getIndustry())).getIndustry());
        }
        // 企业管理员
        // 企业用户

        List<EnterprisesUserDTO> enterprisesUserDTOs = new ArrayList<EnterprisesUserDTO>();
        Integer orgId = enterprisesCheck.getRealId();
        // 如果初次申请，则orgId为空
        if (orgId == null) {
            ArrayList<EnterprisesUserDTO> enterprisesAdmins = Lists.newArrayList();
            AuthPlatformUser u = userMapper.selectOne(new AuthPlatformUser().setUsername(enterprisesCheck.getAdmin()));
            enterprisesAdmins.add(BeanUtils.copyProperties(u, EnterprisesUserDTO.class));
            dto.setEnterprisesAdmins(enterprisesAdmins);
            dto.setUserDTOs(enterprisesUserDTOs);
        } else {
            // 查询企业管理员
            List<AuthPlatformUser> enterpriseAdmins = userEnterpriseService.getEnterpriseAdmin(orgId);
            List<EnterprisesUserDTO> admins = BeanUtils.assemble(EnterprisesUserDTO.class, enterpriseAdmins);
            dto.setEnterprisesAdmins(admins);
            // 查询企业普通用户
            List<AuthPlatformUser> enterpriseUsers = userEnterpriseService.getEnterpriseCommonUser(orgId);
            dto.setUserDTOs(BeanUtils.assemble(EnterprisesUserDTO.class, enterpriseUsers));
        }
        // 附件信息
        // 营业执照附件列表
        List<EnterprisesCheckAttachment> enclosuresList = enterprisesCheckAttachmentMapper
                .selectList(new EntityWrapper<EnterprisesCheckAttachment>().eq("enterprises_check_id", id)
                        .eq("type", EnumAttacheType.type.enclosure.getKey())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        // 营业许可证附件列表
        List<EnterprisesCheckAttachment> permitsList = enterprisesCheckAttachmentMapper
                .selectList(new EntityWrapper<EnterprisesCheckAttachment>().eq("enterprises_check_id", id)
                        .eq("type", EnumAttacheType.type.permit.getKey())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        // 企业认证授权书附件列表
        List<EnterprisesCheckAttachment> certificatesList = enterprisesCheckAttachmentMapper
                .selectList(new EntityWrapper<EnterprisesCheckAttachment>().eq("enterprises_check_id", id)
                        .eq("type", EnumAttacheType.type.certificate.getKey())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        List<EnterprisesAttachmentRQ> one = BeanUtils.assemble(EnterprisesAttachmentRQ.class, enclosuresList);
        List<EnterprisesAttachmentRQ> two = BeanUtils.assemble(EnterprisesAttachmentRQ.class, permitsList);
        List<EnterprisesAttachmentRQ> three = BeanUtils.assemble(EnterprisesAttachmentRQ.class, certificatesList);
        // 组装返回信息
        dto.setEnclosuresList(one).setPermitsList(two).setCertificatesList(three);
        // 开通的产品及角色信息
        List<EnterprisesAppsOpenedDTO> enterprisesAppsOpenedDTOs = enterprisesAppsMapper.queryOpendAppRoles(orgId);
        dto.setAppsDTOs(enterprisesAppsOpenedDTOs);
        return dto;
    }

    /**
     * 审核企业
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult audit(Integer id, Integer type, String reason, AuthPlatformUserInfo userInfo) {
        EnterprisesCheck enterprisesCheck = enterprisesCheckMapper
                .selectOne(new EnterprisesCheck().setId(id).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(enterprisesCheck)) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_CHECK_NOT_EXIST,
                    ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
        }
        // 查询此时企业认证的审核状态
        Integer preType = enterprisesCheck.getType();
        /* 后台消息记录通知人 */
        String childRoleId = configService.getConfigByconfigKey("enterprisesCheck_audit_roleId").getConfigValue();
        List<Integer> managers = mRoleRoleMapper.getManagerIdsByChildId(Integer.valueOf(childRoleId));
        int[] managerIds = managers.stream().mapToInt(Integer::valueOf).toArray();
        // 审核企业认证
        if (EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey().equals(preType)) {
            if (CheckTypeCommon.REFUSED.getKey().equals(type)) {
                enterprisesCheck.setType(EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey());
                enterprisesCheck.setFailureReason(reason);
                enterprisesCheckMapper.updateById(enterprisesCheck);
                AuthPlatformUser user = userMapper.selectOne(new AuthPlatformUser().setUsername(enterprisesCheck.getAdmin()));
                enterprisesCheckLogService.insertEnterprisesCheckLog(enterprisesCheck.getId(),
                        EnumEnterpriseCheck.checkLogType.AUDIT_APPLY.getKey(),
                        EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey(), reason, userInfo, user);
                /* 中台消息 */
                //保存提示消息
                middleSystemNoticeService.createNotice(enterprisesCheck.getCreateId(),
                        EnumMiddleNoticeTitle.title.ENTERPRISE_APPLY_RESULT.getValue(),
                        MiddleNoticeContentTemplate.ENTERPRISE_APPLICATION_REFUSE.getKey(),
                        enterprisesCheck.getName());
                /* 后台消息 */
                // 记录系统消息
                List<SystemNotice> systemNotices = systemNoticeService.createNotice(managerIds,
                        NoticeTemplateType.ENTERPRISE_APPROVAL,
                        EnumEnterpriseCheck.CheckMsgType.REFUSED.getValue(), enterprisesCheck.getName());
                // 后台展示的日志对象
                OperatorLog operatorLog = new OperatorLog().setOperatorId(userInfo.getId())
                        .setOperatorRoleId(userInfo.getRoleList().get(0).getId())
                        .setOperatorRoleName(userInfo.getRoleList().get(0).getRoleType()).setOperatorTime(new Date())
                        .setOperatorContent(CheckTypeCommon.REFUSED.getValue() + enterprisesCheck.getName() + "的入驻申请");
                ;
                operatorLogService.insert(operatorLog);
                // 插入系统通知
                if (!systemNoticeService.insertBatch(systemNotices)) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL,
                            ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
                }
                //发送短信
                sendSmsAfterEnterprisesCheck(enterprisesCheck.getAdmin(), enterprisesCheck.getName(),
                        NoticeTemplateType.ENTERPRISE_APPLICATION_AUDIT_FAILED.getKey());
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }
            if (CheckTypeCommon.PASSED.getKey().equals(type)) {
                // 添加enterprises
                AuthEnterprise enterprise = new AuthEnterprise();
                enterprise.setName(enterprisesCheck.getName());
                enterprise.setAdmin(enterprisesCheck.getAdmin());
                enterprise.setContact(enterprisesCheck.getContact());
                enterprise.setIndustry(Integer.valueOf(enterprisesCheck.getIndustry()));
                enterprise.setLinkman(enterprisesCheck.getLinkman());
                enterprise.setStreet(enterprisesCheck.getStreet());
                enterprise.setRegionid(Integer.valueOf(enterprisesCheck.getRegionid()));
                enterprise.setAddress(enterprisesCheck.getAddress());
                enterprise.setStatus(Status.TRUE.getKey());
                enterprise.setCreateTime(new Date());
                if (enterprisesMapper.insert(enterprise) <= Status.FALSE.getKey()) {
                    log.error("调用{}的{}方法出错，企业信息保存异常", "review", "insert()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
                }
                enterprisesCheck.setType(EnumEnterpriseCheck.CheckType.PASSED_REGISTER.getKey());
                enterprisesCheck.setFailureReason(null);
                enterprisesCheck.setRealId(enterprise.getId());
                enterprisesCheck.setModifier(userInfo.getNickname());
                enterprisesCheck.setModifyId(userInfo.getId());
                enterprisesCheck.setUpdateAt(new Date());
                enterprisesCheckMapper.updateById(enterprisesCheck);
                // 用户企业信息
                AuthPlatformUser user = userMapper.selectOne(new AuthPlatformUser().setPhone(enterprisesCheck.getAdmin()));
                enterprisesCheckLogService.insertEnterprisesCheckLog(enterprisesCheck.getId(),
                        EnumEnterpriseCheck.checkLogType.AUDIT_APPLY.getKey(),
                        EnumEnterpriseCheck.CheckType.PASSED_REGISTER.getKey(), reason, userInfo, user);
                // 处理附件信息
                List<EnterprisesCheckAttachment> attachmentList = enterprisesCheckAttachmentMapper
                        .selectList(new EntityWrapper<EnterprisesCheckAttachment>()
                                .eq("enterprises_check_id", enterprisesCheck.getId())
                                .eq("status", Status.TRUE.getKey()));
                for (EnterprisesCheckAttachment enterprisesCheckAttachment : attachmentList) {
                    AuthCommonFile commonFile = new AuthCommonFile();
                    commonFile.setEnterprisesId(enterprise.getId())
                            .setType(enterprisesCheckAttachment.getType())
                            .setName(enterprisesCheckAttachment.getFileName())
                            .setUrl(enterprisesCheckAttachment.getFileUrl())
                            .setStatus(Status.TRUE.getKey())
                            .setCreateTime(new Date())
                            .setCreateId(enterprisesCheckAttachment.getCreateId());
                    commonFileService.insert(commonFile);
                }
//                AuthPlatformUserEnterprise enterprisesUsers = new AuthPlatformUserEnterprise();
//                enterprisesUsers.setUserId(user.getId());
//                enterprisesUsers.setEnterpriseId(enterprise.getId());
//                enterprisesUsers.setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
//                        .setStatus(EnumCommon.IsActive.is_active.getKey())
//                        .setCreateTime(new Date())
//                        .setCreateUser(user.getId());
//                if (userEnterpriseMapper.insert(enterprisesUsers) <= Status.FALSE.getKey()) {
//                    log.error("调用{}的{}方法出错，企业用户关系信息保存异常", "review", "insert()");
//                    throw new BusinessException(ResCodeEnum.SAVE_FAIL,
//                            ExceptionMessageEnum.USER_ENTERPRISE_RELATION_SAVE_FAILED);
//                }
                //在用户角色表中新增数据
                UserRole userRole = new UserRole().setRoleId(EnumRole.RoleType.superAdmin.getKey())
                        .setUserId(user.getId()).setOrgId(enterprise.getId());
                usersRolesMapper.insert(userRole);
                // 创建企业的同时创建一个同名的部门在该企业下
                Departments department = new Departments();
                department.setName(enterprisesCheck.getName());
                department.setOrgId(enterprise.getId());
                department.setCreateAt(new Date());
                department.setLevel(EnumEnterpriseLevel.levelNode.first_level.getKey());
                if (departmentsMapper.insert(department) <= Status.FALSE.getKey()) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
                }
                // 添加部门tree_id
                department.setTreeId(department.getId());
                if (departmentsMapper.updateById(department) <= Status.FALSE.getKey()) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
                }
                // 将管理员添加到该部门下
                UsersDepartments usersDepartments = new UsersDepartments();
                usersDepartments.setDepartmentId(department.getId());
                usersDepartments.setUserId(user.getId());
                if (usersDepartmentsMapper.insert(usersDepartments) <= Status.FALSE.getKey()) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL,
                            ExceptionMessageEnum.USER_DEPARTMENT_RELATION_SAVE_FAILED);
                }
                /* 中台消息 */
                //保存提示消息
                middleSystemNoticeService.createNotice(enterprisesCheck.getCreateId(),
                        EnumMiddleNoticeTitle.title.ENTERPRISE_APPLY_RESULT.getValue(),
                        MiddleNoticeContentTemplate.ENTERPRISE_APPLY_PASS.getKey(),
                        enterprisesCheck.getName());
                /* 后台消息 */
                // 记录系统消息
                List<SystemNotice> systemNotices = systemNoticeService.createNotice(managerIds,
                        NoticeTemplateType.ENTERPRISE_APPROVAL,
                        EnumEnterpriseCheck.CheckMsgType.PASSED.getValue(), enterprisesCheck.getName());
                // 插入系统通知
                if (!systemNoticeService.insertBatch(systemNotices)) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL,
                            ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
                }
                // 后台展示的日志对象
                OperatorLog operatorLog = new OperatorLog().setOperatorId(userInfo.getId())
                        .setOperatorRoleId(userInfo.getRoleList().get(0).getId())
                        .setOperatorRoleName(userInfo.getRoleList().get(0).getRoleType()).setOperatorTime(new Date())
                        .setOperatorContent(CheckTypeCommon.PASSED.getValue() + enterprisesCheck.getName() + "的入驻申请");
                ;
                operatorLogService.insert(operatorLog);
                //发送短信
                sendSmsAfterEnterprisesCheck(enterprisesCheck.getAdmin(), enterprisesCheck.getName(),
                        NoticeTemplateType.ENTERPRISE_APPLICATION_AUDIT_APPROVED.getKey());
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }
        }
        // 企业信息修改注册
        if (EnumEnterpriseCheck.CheckType.IN_AUDIT_MODIFY.getKey().equals(preType)) {
            if (CheckTypeCommon.REFUSED.getKey().equals(type)) {
                enterprisesCheck.setType(EnumEnterpriseCheck.CheckType.REFUSED_MODIFY.getKey());
                enterprisesCheck.setFailureReason(reason);
                enterprisesCheck.setModifier(userInfo.getNickname());
                enterprisesCheck.setModifyId(userInfo.getId());
                enterprisesCheck.setUpdateAt(new Date());
                enterprisesCheckMapper.updateById(enterprisesCheck);
                /* 后台消息 */
                // 记录系统消息
                List<SystemNotice> systemNotices = systemNoticeService.createNotice(managerIds,
                        NoticeTemplateType.ENTERPRISE_INFO_UPDATE_RESULT,
                        EnumEnterpriseCheck.CheckMsgType.REFUSED.getValue(), enterprisesCheck.getName());
                // 插入系统通知
                if (!systemNoticeService.insertBatch(systemNotices)) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL,
                            ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }
            if (CheckTypeCommon.PASSED.getKey().equals(type)) {
                // 修改enterprisesCheck信息
                enterprisesCheck.setType(EnumEnterpriseCheck.CheckType.PASSED_MODIFY.getKey());
                enterprisesCheck.setFailureReason(null);
                enterprisesCheckMapper.updateById(enterprisesCheck);
                // 修改enterprises信息
                AuthEnterprise enterprise = enterprisesMapper.selectById(enterprisesCheck.getRealId());
                String org_name = enterprise.getName();
                enterprise.setName(enterprisesCheck.getName());
                enterprise.setAdmin(enterprisesCheck.getAdmin());
                enterprise.setContact(enterprisesCheck.getContact());
                enterprise.setIndustry(Integer.valueOf(enterprisesCheck.getIndustry()));
                enterprise.setLinkman(enterprisesCheck.getLinkman());
                enterprise.setStreet(enterprisesCheck.getStreet());
                enterprise.setRegionid(Integer.valueOf(enterprisesCheck.getRegionid()));
                enterprise.setAddress(enterprisesCheck.getAddress());
                enterprise.setStatus(Status.TRUE.getKey());
                enterprise.setUpdateTime(new Date());
                if (enterprisesMapper.updateById(enterprise) <= Status.FALSE.getKey()) {
                    log.error("调用{}的{}方法出错，企业信息保存异常", "review", "insert()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
                }
                // 修改附件信息
                // 老附件软删除
                List<AuthCommonFile> attachments = commonFileService
                        .selectList(new EntityWrapper<AuthCommonFile>().eq("enterprises_id", enterprise.getId())
                                .eq("status", Status.TRUE.getKey()));
                for (AuthCommonFile enterprisesAttachment : attachments) {
                    enterprisesAttachment.setStatus(Status.FALSE.getKey());
                    commonFileService.updateById(enterprisesAttachment);
                }
                // 新附件插入
                List<EnterprisesCheckAttachment> attachmentList = enterprisesCheckAttachmentMapper
                        .selectList(new EntityWrapper<EnterprisesCheckAttachment>()
                                .eq("enterprises_check_id", enterprisesCheck.getId())
                                .eq("status", Status.TRUE.getKey()));
                for (EnterprisesCheckAttachment enterprisesCheckAttachment : attachmentList) {
                    AuthCommonFile commonFile = new AuthCommonFile();
                    commonFile.setEnterprisesId(enterprise.getId())
                            .setType(enterprisesCheckAttachment.getType())
                            .setName(enterprisesCheckAttachment.getFileName())
                            .setUrl(enterprisesCheckAttachment.getFileUrl())
                            .setStatus(Status.TRUE.getKey())
                            .setCreateTime(new Date())
                            .setCreateId(enterprisesCheckAttachment.getCreateId());
                    commonFileService.insert(commonFile);
                }
                // 修改department信息，第一个部门信息（原以公司名创建的部门，修改其信息）
                List<Departments> departments = departmentsMapper.selectList(
                        new EntityWrapper<Departments>().eq("org_id", enterprise.getId()).and().eq("name", org_name));
                if (!CollectionUtils.isEmpty(departments)) {
                    departments.get(0).setName(enterprisesCheck.getName());
                    if (departmentsMapper.updateById(departments.get(0)) <= Status.FALSE.getKey()) {
                        throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
                    }
                }
                /* 后台消息 */
                // 记录系统消息
                List<SystemNotice> systemNotices = systemNoticeService.createNotice(managerIds,
                        NoticeTemplateType.ENTERPRISE_INFO_UPDATE_RESULT,
                        EnumEnterpriseCheck.CheckMsgType.PASSED.getValue(), enterprisesCheck.getName());
                // 插入系统通知
                if (!systemNoticeService.insertBatch(systemNotices)) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL,
                            ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATA_FAIL);
    }

    @Override
    public EnterpriseRegisterInfoDTO getEnterpriseApplyDetail(Integer id) {
        // 企业审核记录
        log.info("查询企业审核记录详细信息");
        EnterprisesCheck enterprisesCheck = enterprisesCheckMapper.selectById(id);
        EnterpriseRegisterInfoDTO dto = BeanUtils.copyProperties(enterprisesCheck, EnterpriseRegisterInfoDTO.class);
        log.info("查询企业营业执照附件列表");
        // 营业执照附件列表
        List<EnterprisesCheckAttachment> enclosuresList = enterprisesCheckAttachmentMapper
                .selectList(new EntityWrapper<EnterprisesCheckAttachment>().eq("enterprises_check_id", id)
                        .eq("type", EnumAttacheType.type.enclosure.getKey())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        log.info("查询企业营业许可证附件列表");
        // 营业许可证附件列表
        List<EnterprisesCheckAttachment> permitsList = enterprisesCheckAttachmentMapper
                .selectList(new EntityWrapper<EnterprisesCheckAttachment>().eq("enterprises_check_id", id)
                        .eq("type", EnumAttacheType.type.permit.getKey())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        log.info("查询企业企业认证授权书附件列表");
        // 企业认证授权书附件列表
        List<EnterprisesCheckAttachment> certificatesList = enterprisesCheckAttachmentMapper
                .selectList(new EntityWrapper<EnterprisesCheckAttachment>().eq("enterprises_check_id", id)
                        .eq("type", EnumAttacheType.type.certificate.getKey())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        List<EnterprisesAttachmentRQ> one = BeanUtils.assemble(EnterprisesAttachmentRQ.class, enclosuresList);
        List<EnterprisesAttachmentRQ> two = BeanUtils.assemble(EnterprisesAttachmentRQ.class, permitsList);
        List<EnterprisesAttachmentRQ> three = BeanUtils.assemble(EnterprisesAttachmentRQ.class, certificatesList);
        // 组装返回信息
        dto.setEnclosuresList(one).setPermitsList(two).setCertificatesList(three);

        return dto;
    }

    /**
     * 获取用户企业申请列表
     *
     * @param userInfo 用户信息
     * @param page     分页对象
     * @return 企业申请列表
     */
    @Override
    public ResponseResult<List<UserApplyEnterprisesDTO>> getUserApplyList(AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        List<Integer> types = Lists.newArrayList(0, 2);
        // 分页查询用户企业审核非已通过的记录列表
        List<EnterprisesCheck> checkList = enterprisesCheckMapper.selectPage(pagination,
                new EntityWrapper<EnterprisesCheck>().eq("create_id", userInfo.getId())
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()).in("type", types)
                        .orderBy("create_at", false));
        List<UserApplyEnterprisesDTO> dtoList = BeanUtils.assemble(UserApplyEnterprisesDTO.class, checkList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList, PageUtils.transToPage(pagination));
    }

    /**
     * @Description 企业申请审批操作后发送短信
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    private void sendSmsAfterEnterprisesCheck(String phone, String enterpriseName, Integer key) {
        try {
            if (!ValidateUtils.isPhone(phone)) {
                log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
            }
            // 组装参数
            JSONObject sendJson = new JSONObject();
            sendJson.put("enterprise_name", enterpriseName);
            smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());
        } catch (Exception e) {
            log.info(e.getMessage());
        }
    }

    /*@Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult modifyAdmin(Integer checkId, Integer userId, ManagerInfo userInfo) {
        EnterprisesCheck enterprisesCheck = enterprisesCheckMapper.selectOne(new EnterprisesCheck().setId(checkId));
        if (ObjectUtils.isEmpty(enterprisesCheck)) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_CHECK_NOT_EXIST,
                    ExceptionMessageEnum.ENTERPRISE_CHECK_NOT_EXIST);
        }
        // 原管理员
        EnterprisesUsers enterprisesUsers = enterprisesUsersMapper.selectOne(new EnterprisesUsers()
                .setEnterpriseId(enterprisesCheck.getRealId()).setRoleId(EnumRole.RoleType.superAdmin.getKey())
                .setIsActive(EnumEnterpriseUser.ActiveType.is_active.getKey()));
        if (ObjectUtils.isEmpty(enterprisesUsers)) {
            throw new BusinessException(ResCodeEnum.ADMIN_NOT_FOUND, ExceptionMessageEnum.ADMIN_NOT_FOUND);
        }
        enterprisesUsers.setRoleId(EnumRole.RoleType.user.getKey());
        enterprisesUsers.setPost(EnumRole.RoleType.user.getValue());
        enterprisesUsersMapper.updateById(enterprisesUsers);
        // 现有管理员
        EnterprisesUsers User = enterprisesUsersMapper
                .selectOne(new EnterprisesUsers().setEnterpriseId(enterprisesCheck.getRealId()).setUserId(userId)
                        .setIsActive(EnumEnterpriseUser.ActiveType.is_active.getKey()));
        User.setRoleId(EnumRole.RoleType.superAdmin.getKey());
        User.setPost(EnumRole.RoleType.superAdmin.getValue());
        enterprisesUsersMapper.updateById(User);
        //修改user_role表
        UserRole oldUserRole = new UserRole().setRoleId(EnumRole.RoleType.user.getKey())
                .setUserId(enterprisesUsers.getUserId()).setOrgId(enterprisesUsers.getEnterpriseId());
        UserRole nowUserRole = new UserRole().setRoleId(EnumRole.RoleType.superAdmin.getKey())
                .setUserId(User.getUserId()).setOrgId(User.getEnterpriseId());
        usersRolesMapper.updateUserRole(oldUserRole);
        usersRolesMapper.updateUserRole(nowUserRole);
        *//* 后台消息 *//*
        // 记录系统消息
        SystemNotice firstNotice = systemNoticeService.createNotice(userInfo.getManagerId(),
                NoticeTemplateType.UPDATE_ENTERPRISE_MANAGER, enterprisesCheck.getName(), User.getNickname());
        // 插入系统通知
        if (!(systemNoticeService.insert(firstNotice))) {
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
        }
        *//* 中台消息-通知原管理员 *//*
        MiddleSystemNoticeRQ secondRq = new MiddleSystemNoticeRQ();
        secondRq.setNotifierId(enterprisesUsers.getId());
        secondRq.setTitle(EnumMiddleNoticeTitle.title.UPDATE_ENTERPRISE_MANAGER.getValue());
        SystemNotice secondNotice = systemNoticeService.createNotice(enterprisesUsers.getId(),
                NoticeTemplateType.UPDATE_ENTERPRISE_MANAGER_OLD, enterprisesCheck.getName());
        secondRq.setContent(secondNotice.getNoticeContent());
        middleSystemNoticeService.saveNotice(secondRq);
        *//* 中台消息-通知现管理员 *//*
        MiddleSystemNoticeRQ thirdRq = new MiddleSystemNoticeRQ();
        thirdRq.setNotifierId(enterprisesUsers.getId());
        thirdRq.setTitle(EnumMiddleNoticeTitle.title.UPDATE_ENTERPRISE_MANAGER.getValue());
        SystemNotice thirdNotice = systemNoticeService.createNotice(enterprisesUsers.getId(),
                NoticeTemplateType.UPDATE_ENTERPRISE_MANAGER_OLD, enterprisesCheck.getName());
        thirdRq.setContent(thirdNotice.getNoticeContent());
        middleSystemNoticeService.saveNotice(thirdRq);
        // 后台展示的日志对象
        OperatorLog operatorLog = new OperatorLog().setOperatorId(userInfo.getManagerId())
                .setOperatorRoleId(userInfo.getRoleInfo().getRoleId())
                .setOperatorRoleName(userInfo.getRoleInfo().getRoleName()).setOperatorTime(new Date())
                .setOperatorContent(enterprisesCheck.getName() + "的管理员更换为" + User.getNickname());
        operatorLogService.insert(operatorLog);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }*/
}

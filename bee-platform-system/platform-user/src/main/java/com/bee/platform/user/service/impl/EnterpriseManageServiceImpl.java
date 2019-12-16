package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.exceptions.ClientException;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.business.dto.AllCheckTypeDTO;
import com.bee.platform.business.dto.CheckTypeDTO;
import com.bee.platform.business.dto.EnterprisesWithAppDTO;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dao.mapper.AuthEnterpriseMapper;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.dao.mapper.EnterprisesCheckMapper;
import com.bee.platform.user.dao.mapper.EnterprisesMapper;
import com.bee.platform.user.email.MailService;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.service.EnterpriseManageService;
import com.bee.platform.user.service.EnterprisesUsersService;
import com.bee.platform.user.service.MiddleSystemNoticeService;
import com.bee.platform.user.service.UsersService;
import com.bee.platform.user.sms.SmsService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-05-07
 */

@Slf4j
@Service
public class EnterpriseManageServiceImpl extends ServiceImpl<AuthEnterpriseMapper, AuthEnterprise> implements EnterpriseManageService {

    @Autowired
    private EnterprisesCheckMapper enterprisesCheckMapper;
    @Autowired
    private UsersService usersService;
    @Autowired
    private AuthPlatformUserEnterpriseService userEnterpriseService;
    @Autowired
    private MailService mailService;
    @Autowired
    private SmsService smsService;
    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;

    @Override
    public ResponseResult<AllCheckTypeDTO> getTypeWithCount(AuthPlatformUserInfo managerInfo) {
        AllCheckTypeDTO result = new AllCheckTypeDTO();
        List<CheckTypeDTO> list = new ArrayList<>();
        //未审核(申请+修改)
        CheckTypeDTO audit = new CheckTypeDTO();
        Integer auditKey = EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey();
        Integer newKey = EnumEnterpriseCheck.CheckType.IN_AUDIT_MODIFY.getKey();
        List<Integer> auditKeys = Arrays.asList(auditKey,newKey);
        audit.setTypeId(EnumEnterpriseCheck.CheckTypeCommon.IN_AUDIT.getKey());
        audit.setTypeName(EnumEnterpriseCheck.CheckTypeCommon.IN_AUDIT.getValue());
        Integer auditCount = enterprisesCheckMapper.selectCount(new EntityWrapper<EnterprisesCheck>().in("type", auditKeys));
        audit.setCount(auditCount);
        list.add(audit);
        //未通过(申请+修改)
        CheckTypeDTO refuse = new CheckTypeDTO();
        Integer refused1 = EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey();
        Integer refused2 = EnumEnterpriseCheck.CheckType.REFUSED_MODIFY.getKey();
        List<Integer> refuseKeys = Arrays.asList(refused1,refused2);
        refuse.setTypeId(EnumEnterpriseCheck.CheckTypeCommon.REFUSED.getKey());
        refuse.setTypeName(EnumEnterpriseCheck.CheckTypeCommon.REFUSED.getValue());
        Integer refuseCount = enterprisesCheckMapper.selectCount(new EntityWrapper<EnterprisesCheck>().in("type", refuseKeys));
        refuse.setCount(refuseCount);
        list.add(refuse);
        //已通过(申请+修改)
        CheckTypeDTO passed = new CheckTypeDTO();
        Integer passed1 = EnumEnterpriseCheck.CheckType.PASSED_REGISTER.getKey();
        Integer passed2 = EnumEnterpriseCheck.CheckType.PASSED_MODIFY.getKey();
        List<Integer> passKeys = Arrays.asList(passed1,passed2);
        passed.setTypeId(EnumEnterpriseCheck.CheckTypeCommon.PASSED.getKey());
        passed.setTypeName(EnumEnterpriseCheck.CheckTypeCommon.PASSED.getValue());
        Integer passedCount = enterprisesCheckMapper.selectCount(new EntityWrapper<EnterprisesCheck>().in("type", passKeys));
        passed.setCount(passedCount);
        list.add(passed);
        result.setList(list);
        result.setTotalCount(auditCount+refuseCount+passedCount);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    @Override
    public ResponseResult<List<EnterprisesWithAppDTO>> getEnterpriseList(AuthPlatformUserInfo managerInfo, Integer typeId, String name, Page page) {
        Pagination pagination= PageUtils.transFromPage(page);
        EntityWrapper<EnterprisesCheck> wrapper = new EntityWrapper<>();
        wrapper.eq("status", EnumCommon.LogicStatus.NORMAL.getKey());
        //审核结果类型
        Integer refuseKey = EnumEnterpriseCheck.CheckTypeCommon.REFUSED.getKey();
        Integer passKey = EnumEnterpriseCheck.CheckTypeCommon.PASSED.getKey();
        Integer auditKey = EnumEnterpriseCheck.CheckTypeCommon.IN_AUDIT.getKey();
        if(!ObjectUtils.isEmpty(typeId)){
            Integer rr = EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey();
            Integer rm = EnumEnterpriseCheck.CheckType.REFUSED_MODIFY.getKey();
            if(refuseKey.equals(typeId)){
                List<Integer> refuseKeys = Arrays.asList(rr, rm);
                wrapper.in("type",refuseKeys);
            }
            Integer pr = EnumEnterpriseCheck.CheckType.PASSED_REGISTER.getKey();
            Integer pm = EnumEnterpriseCheck.CheckType.PASSED_MODIFY.getKey();
            if(passKey.equals(typeId)){
                List<Integer> passKeys = Arrays.asList(pr, pm);
                wrapper.in("type",passKeys);
            }
            Integer ar = EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey();
            Integer am = EnumEnterpriseCheck.CheckType.IN_AUDIT_MODIFY.getKey();
            if(auditKey.equals(typeId)){
                List<Integer> auditKeys = Arrays.asList(ar, am);
                wrapper.in("type",auditKeys);
            }
        }
        //公司名
        if(!StringUtils.isEmpty(name)){
            wrapper.like("name",name);
        }
        List<EnterprisesCheck> checks = enterprisesCheckMapper.selectPage(pagination, wrapper.orderBy("create_at",false));
        List<EnterprisesWithAppDTO> result = BeanUtils.assemble(EnterprisesWithAppDTO.class, checks).stream()
                .map(obj -> obj.setTypeName(EnumEnterpriseCheck.CheckType.loanCheckType(obj.getType()).getValue()))
                .collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    /**
     * @notes: 企业管理员重置企业用户密码
     * @Author: junyang.li
     * @Date: 14:54 2019/5/9
     * @param userInfo : 操作人信息
     * @param userId : 被操作人id
     * @param type : 通知类型
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> resetPassword(AuthPlatformUserInfo userInfo, Integer userId, Integer type) {
        //查询该用户是否存在
        User  user=usersService.selectById(userId);
        if(user==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //查询该用户是否是当前企业成员
        AuthPlatformUserEnterprise userEnterprise=userEnterpriseService.selectOne(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setEnterpriseId(userInfo.getOrgId())
                .setUserId(userId)));
        if(userEnterprise==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_USER_NOT_FOUND);
        }
        //随机生成6位密码
        String password= RandomStringUtils.randomAlphanumeric(6);
        //加密
        String encode= BCryptPassword.encode(password);
        //修改密码
        User nowUser=new User().setId(userId)
                .setPassword(encode).setUpdateId(userInfo.getId())
                .setUpdateAt(new Date());
        usersService.updateById(nowUser);
        //List<SystemNotice> list=addSystemNotice(managerInfo,manager);
        //插入系统通知
        //systemNoticeService.insertAll(list);
        //发送短信给用户新的密码
        if(Status.FALSE.getKey().equals(type)){

            //发送提示短信
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("enterprise_admin_username",userInfo.getNickname() );
            jsonObject.put("password", password);
            sendMessageForPrompt(user.getUsername(), NoticeTemplateType.ENTERPRISE_ADMIN_RESET_PASSWORD.getKey(), jsonObject);
            //保存消息
            middleSystemNoticeService.createNotice(user.getId(),
                    EnumMiddleNoticeTitle.title.MANGER_CHANGE_PASSWORD.getValue(),
                    MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_RESET_PASSWORD.getKey(),
                    userInfo.getNickname(), password);
            middleSystemNoticeService.createNotice(user.getId(),
                    EnumMiddleNoticeTitle.title.MANGER_CHANGE_PASSWORD.getValue(),
                    MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_RESET_PASSWORD_NOTICE.getKey(),
                    userInfo.getNickname(), user.getUsername().substring(7));
            //保存通知企业管理员的消息
            middleSystemNoticeService.createNotice(userInfo.getId(),
                    EnumMiddleNoticeTitle.title.MANGER_CHANGE_PASSWORD.getValue(),
                    MiddleNoticeContentTemplate.RESET_PASSWORD.getKey(),
                    user.getNickname());
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //发送邮件
        mailService.sendMail(user.getEmail());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 管理员重置后台账号的密码创建系统通知
     * @Author: junyang.li
     * @Date: 14:33 2019/5/9
     * @param userInfo : 操作人信息
     * @param user : 被操作人信息
     * @return: java.util.List<com.bee.platform.user.entity.SystemNotice>
     */
    private List<SystemNotice> addSystemNotice(AuthPlatformUserInfo userInfo,User  user){
        /*//生成操作人系统通知
        SystemNotice operator=systemNoticeService.createNotice(managerInfo.getManagerId(),
                NoticeTemplateType.RESET_PASSWORD,manager.getUsername());
        //被操作人的系统通知
        SystemNotice notice=systemNoticeService.createNotice(manager.getManagerId(),
                NoticeTemplateType.RESET_PASSWORD_USER, CommonUtils.getPhoneTailNum(manager.getUsername()));
        List<SystemNotice> list=new ArrayList<>();
        list.add(operator);
        list.add(notice);
        return list;*/
        return null;
    }

	@Override
	public ResponseResult cancelApply(Integer checkId, AuthPlatformUserInfo userInfo) {
		EnterprisesCheck check = enterprisesCheckMapper.selectById(checkId);
		check.setStatus(Status.FALSE.getKey());
		check.setUpdateAt(new Date());
		check.setModifier(userInfo.getNickname());
		check.setModifyId(userInfo.getId());
        if (enterprisesCheckMapper.updateById(check) <= Status.FALSE.getKey()) {
            throw new BusinessException(ResCodeEnum.CANCEL_FAIL, ExceptionMessageEnum.CANCEL_FAIL);
        }
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
	}
    private void sendMessageForPrompt(String phone, Integer key, JSONObject jsonObject) {
        try {
            smsService.sendMessageForPrompt(phone, key, jsonObject.toJSONString());
        } catch (ClientException e) {
            log.error("手机提示消息发送失败，手机号是:{}。错误信息是:{}", phone, e);
        }
    }
}

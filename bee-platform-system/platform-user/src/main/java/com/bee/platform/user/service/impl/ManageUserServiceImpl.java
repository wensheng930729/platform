package com.bee.platform.user.service.impl;

import com.aliyuncs.exceptions.ClientException;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.entity.MRoleInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.*;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.*;
import com.bee.platform.user.dao.mapper.MPlatformManagersMapper;
import com.bee.platform.user.dto.ManagerUsersDTO;
import com.bee.platform.user.email.MailService;
import com.bee.platform.user.entity.ManagersRoles;
import com.bee.platform.user.entity.OperatorLog;
import com.bee.platform.user.entity.PlatformManagers;
import com.bee.platform.user.entity.SystemNotice;
import com.bee.platform.user.service.*;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.CenerateIdUtils;
import com.bee.platform.user.vo.PlatformManagerEditVO;
import com.bee.platform.user.vo.PlatformManagerVO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import redis.clients.jedis.exceptions.JedisConnectionException;
import redis.clients.jedis.exceptions.JedisException;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @description: 管理后台的用户管理相关接口
 * @author: junyang.li
 * @create: 2019-04-25 14:12
 **/
@Slf4j
@Service
public class ManageUserServiceImpl implements ManageUserService {

    @Autowired
    private MPlatformManagersMapper managersMapper;

    @Autowired
    private SmsService smsService;

    @Autowired
    private MailService mailService;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private ConfigService configService;

    @Autowired
    private ManagersRolesService managersRolesService;

    @Autowired
    private SystemNoticeService systemNoticeService;

    @Autowired
    private CenerateIdUtils cenerateIdUtils;

    @Autowired
    private MRolesService rolesService;

    @Autowired
    private OperatorLogService operatorLogService;

    /**
     * @notes 获得后台用户的登录信息
     * @Author junyang.li
     * @Date 10:37 2019/4/30
     **/
    @Override
    public ManagerInfo getManagerInfo(String sysToken) {
        //判空
        if(StringUtils.isBlank(sysToken)){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.USER_NOT_LEGAL);
        }
        try {
            ManagerInfo managerInfo=jedisService.getObject(sysToken,ManagerInfo.class);
            if(managerInfo==null){
                //从数据中查询该用户信息
                managerInfo=getManagerByToken(sysToken);
                int val=Integer.valueOf(configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN, "7200", "Token 在redis在的失效时间"));
                Date expiresIn= DateUtil.plusSeconds((val));
                managerInfo.setExpiresIn(expiresIn);
                //再插入到缓存中
                jedisService.setObject(managerInfo.getSysToken(), managerInfo,val);
                return managerInfo;
            }
            return managerInfo;
        }catch (JedisConnectionException e){
            log.error("getManagerInfo方法redis连接异常,异常信息是：{}",e);
            return getManagerByToken(sysToken);
        }
    }

    /**
     * @notes: 通过用户id获得用户详细信息
     * @Author: junyang.li
     * @Date: 11:15 2019/5/10
     * @param managerId :
     * @return: com.bee.platform.common.entity.ManagerInfo
     */
    @Override
    public ManagerInfo getManagerDetail(Integer managerId) {
        PlatformManagers manager=managersMapper.selectById(managerId);
        if(manager==null){
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NOT_EXIST);
        }
        //查询操作人
        Integer updateId=manager.getUpdateId()==null?manager.getCreatorId():manager.getUpdateId();
        String updateName=managersMapper.getUpdatorNickename(updateId);
        ManagerInfo info=BeanUtils.copyProperties(manager,ManagerInfo.class)
                .setUpdateId(updateId).setUpdateName(updateName).setUpdateTime(manager.getUpdateAt());
        //查询用户角色
        MRoleInfo roleInfo=managersRolesService.getUserRoles(managerId);
        return info.setRoleInfo(roleInfo==null?new MRoleInfo():roleInfo);
    }

    /**
     * @notes  管理后台编辑用户
     * @Author junyang.li
     * @Date 14:13 2019/4/25
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> editUser(ManagerInfo managerInfo, PlatformManagerVO vo) {

        //查询角色是否存在
        MRoleInfo roleInfo=rolesService.getRolesById(vo.getRoleId(), ManagerRoleType.COMMON);
        Integer superRoleId=managerInfo.getRoleInfo().getRoleId();
        //如果不为正常角色或者超级管理员
        if(roleInfo==null ){
            if(!superRoleId.equals(vo.getRoleId())){
                log.error("编辑用户时传入的角色id未查询到对应的角色组。roleId={}",vo.getRoleId());
                return ResponseResult.buildResponseResult(ResCodeEnum.MANAGER_ROLE_NOT_FOUND);
            }
            roleInfo=managerInfo.getRoleInfo();
        }
        //如果id为空，则为新增用户
        if(vo.getManagerId()==null){
            return insertManager(managerInfo,vo,roleInfo);
        }
        //编辑用户，查出该用户
        PlatformManagers manager=managersMapper.selectOne(new PlatformManagers().setManagerId(vo.getManagerId()));
        if(manager==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //判断是否需要发系统通知和账户是否有重复
        this.isRepeat(manager,vo);
        //查询用户原有角色
        MRoleInfo managerRole=managersRolesService.getUserRoles(manager.getManagerId());
        //组装数据
        PlatformManagers newManager=BeanUtils.copyProperties(vo,PlatformManagers.class)
                .setManagerId(manager.getManagerId()).setUpdateId(managerInfo.getManagerId())
                .setUpdateAt(new Date());
        //改变角色
        List<SystemNotice> list=new ArrayList<>();
        String name=CommonUtils.getNickName(vo.getNickname(),manager.getUsername());
        String content=operatorLogService.createContent(OperatorLogType.UPDATE_USER_INFO,
                managerInfo.getNickname(),name);
        if(!roleInfo.getRoleId().equals(managerRole.getRoleId())){
            //删除原有角色
            managersRolesService.delete(new EntityWrapper<ManagersRoles>().where("manager_id={0}",manager.getManagerId()));
            //新增角色
            ManagersRoles roles=new ManagersRoles().setManagerId(manager.getManagerId()).setRoleId(roleInfo.getRoleId());
            managersRolesService.insert(roles);
            SystemNotice roleNotice=systemNoticeService.createNotice(managerInfo.getManagerId(),
                    NoticeTemplateType.UPDATE_AUTH_USER,name);
            list.add(roleNotice);
            //原来的角色名称
            MRoleInfo oldRole=rolesService.getRoleByRoleId(managerRole.getRoleId());
            //操作日志
            content+=operatorLogService.createContent(OperatorLogType.UPDATE_ROLE,
                    oldRole.getRoleName(),roleInfo.getRoleName());
        }
        Integer result=managersMapper.updateById(newManager);
        if(result>0){
            SystemNotice notice=systemNoticeService.createNotice(managerInfo.getManagerId(),
                    NoticeTemplateType.MANAGER_UPDATE_USER,name);
            list.add(notice);
        }
        operatorLogService.insetOperatorLog(managerInfo,content);
        //新增手机号修改通知
        systemNoticeService.insertAll(list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * @notes: 新增管理用户
     * @Author: junyang.li
     * @Date: 17:25 2019/5/8
     * @param managerInfo : 操作人信息
     * @param vo :  新增用户信息
     * @param roleInfo :  角色信息
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    private ResponseResult<ResCodeEnum>  insertManager(ManagerInfo managerInfo, PlatformManagerVO vo,MRoleInfo roleInfo){
        this.isRepeat(new PlatformManagers(),vo);
        //生成用户编号
        String defaultPassword= configService.getConfValue(ConstantsUtil.DEFAULT_PASSWORD,"123456","新增用户时的默认密码，默认123456配置表中可配置");
        String password= BCryptPassword.encode(defaultPassword);
        PlatformManagers managerUser= BeanUtils.copyProperties(vo,PlatformManagers.class).setBeesrvId(this.createBeesrvId())
                .setCreateAt(new Date()).setUpdateAt(new Date()).setCreatorId(managerInfo.getManagerId())
                .setPassword(password).setStatus(Status.TRUE.getKey());
        //插入用户
        managersMapper.insert(managerUser);
        //查出该用户id
        PlatformManagers newManager=managersMapper.selectOne(new PlatformManagers().setUsername(vo.getUsername()));
        //为该用户插入角色
        ManagersRoles role=new ManagersRoles().setRoleId(vo.getRoleId()).setManagerId(newManager.getManagerId());
        //插入角色
        managersRolesService.insert(role);
        //账号名称为空
        String name=CommonUtils.getNickName(managerUser.getNickname(),managerUser.getUsername());
        String content= operatorLogService.createContent(OperatorLogType.CREATE_NEW_ACCOUNT,
                managerInfo.getNickname(),name,roleInfo.getRoleName());
        operatorLogService.insetOperatorLog(managerInfo,content);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 判断该用户信息是否发送改变
     * @Author: junyang.li
     * @Date: 9:20 2019/5/9
     * @param old : 旧的用户信息
     * @param now : 新的用户信息
     * @return: com.bee.platform.user.entity.PlatformManagers
     */
    private Wrapper<PlatformManagers> isChange(PlatformManagers old,PlatformManagerVO now){
        Wrapper<PlatformManagers> wrapper=new EntityWrapper<>();
        //账号发生改变
        boolean usernameIsChange=this.strIsChange(old.getUsername(),now.getUsername());
        if(usernameIsChange){
            //校验验证码
            /**
             * 第一版暂时不做短信验证码校验，第二版需要做，所有暂时不删代码
             */
            /*boolean result=smsService.checkVerificationCode(now.getUsername(),now.getPhoneCode());
            if(!result){
                throw new BusinessException(ResCodeEnum.VALIDATE_CODE_ERROR,ExceptionMessageEnum.VALIDATE_CODE_ERROR);
            }*/
            wrapper.or().eq("username",now.getUsername());
        }
        //邮箱发生改变
        boolean emailIsChange=this.strIsChange(old.getEmail(),now.getEmail());
        if(emailIsChange && !StringUtils.isBlank(now.getEmail())){
            wrapper.or().eq("email",now.getEmail());
        }
        //邮箱发生改变
        boolean nicknameIsChange=this.strIsChange(old.getNickname(),now.getNickname());
        if(nicknameIsChange && !StringUtils.isBlank(now.getNickname())){
            wrapper.or().eq("nickname",now.getNickname());
        }
        return usernameIsChange || emailIsChange || nicknameIsChange ? wrapper : null;
    }

    /**
     * @notes: 校验新旧字符串是否发生改变  发生改变返回true ，否在为false
     * @Author: junyang.li
     * @Date: 11:35 2019/5/10
     * @param old : 旧字符串
     * @param now :  新字符串
     * @return: boolean
     */
    private boolean strIsChange(String old,String now){
        //第一个串为空
        if(org.springframework.util.StringUtils.isEmpty(old)){
            //第二个串为空则未改变，第二个串不为空则改变
            return !org.springframework.util.StringUtils.isEmpty(now);
        }
        //第一个串不为空,，第二个串为空，则未改变
        if(org.springframework.util.StringUtils.isEmpty(now)){
            return false;
        }
        //都不为空，则判断是否相同
        return !old.equals(now);
    }
    /**
     * @notes: 编辑用户时检查账户是否有重复
     * @Author: junyang.li
     * @Date: 15:55 2019/5/9
     * @param old : 查询对象
     * @param vo : 传入参数
     * @return: void
     */
    private void isRepeat(PlatformManagers old ,PlatformManagerVO vo){
        Wrapper<PlatformManagers> wrapper=isChange(old,vo);
        if(wrapper ==null ){
            return ;
        }
        List<PlatformManagers> managers=managersMapper.selectList(wrapper);
        //有重复账号
        if(CollectionUtils.isEmpty(managers)){
            return ;
        }
        //不为空取第一对象
        PlatformManagers repeat=managers.get(0);
        if(vo.getUsername().equals(repeat.getUsername())){
            log.error("新增用户时录入的账号已经存在，无法新增用户vo={}，重复的对象是：repeat={}",vo,repeat);
            throw new BusinessException(ResCodeEnum.ACCOUNT_REPETITION,ExceptionMessageEnum.ACCOUNT_REPETITION);
        }
        if(vo.getEmail().equals(repeat.getEmail())){
            log.error("新增用户时录入的邮箱已经存在，无法新增用户，vo={}，重复的对象是：repeat={}",vo,repeat);
            throw new BusinessException(ResCodeEnum.EMAIL_REPETITION,ExceptionMessageEnum.EMAIL_REPETITION);
        }
        log.error("新增用户时录入的昵称已经存在，无法新增用户，vo={}，重复的对象是：repeat={}",vo,repeat);
        throw new BusinessException(ResCodeEnum.ACCOUNT_NAME_REPEAT,ExceptionMessageEnum.ACCOUNT_NAME_REPEAT);
    }

    /**
     * @notes: 个人中心更换手机号获得手机验证码
     * @Author: junyang.li
     * @Date: 15:30 2019/5/5
     * @param managerInfo : 操作人信息
     * @param newPhone : 新的手机号
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    public ResponseResult<ResCodeEnum> getPhoneCheckCode(ManagerInfo managerInfo, String newPhone){
        //查询该手机号是否存在
        PlatformManagers platformManager=managersMapper.selectOne(new PlatformManagers().setUsername(newPhone));
        if(platformManager!=null){
            return ResponseResult.buildResponseResult(ResCodeEnum.PHONE_ALREADY_EXIST);
        }
        //发送验证码
        try {
            smsService.sendMessage(newPhone);
            String param=CommonUtils.getPhoneTailNum(newPhone);
            systemNoticeService.insertNotice(managerInfo.getManagerId(),NoticeTemplateType.SYSTEM_NOTICE_NOTE,param);
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_SUCCESS);
        } catch (ClientException e) {
            log.error("手机验证码发送失败，手机号是:{}。错误信息是:{}",newPhone,e);
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
        }
    }
    /**
     * @notes: 个人中心更换邮箱获得邮箱验证码
     * @Author: junyang.li
     * @Date: 15:30 2019/5/5
     * @param managerInfo : 操作人信息
     * @param newEmail : 新的手机号
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    public ResponseResult<ResCodeEnum> getEmailCheckCode(ManagerInfo managerInfo, String newEmail) {
        //查询该手机号是否存在
        PlatformManagers platformManager=managersMapper.selectOne(new PlatformManagers().setEmail(newEmail));
        if(platformManager!=null){
            return ResponseResult.buildResponseResult(ResCodeEnum.PHONE_ALREADY_EXIST);
        }
        //发送邮箱验证码
        return mailService.sendMail(newEmail);
    }

    /**
     * @notes: 个人中心修改个人资料
     * @Author: junyang.li
     * @Date: 11:41 2019/5/5
     * @param managerInfo : 操作人信息
     * @param editVO : 修改参数
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateManager(ManagerInfo managerInfo, PlatformManagerEditVO editVO) {
        List<SystemNotice> list=new ArrayList<>();
        String date=DateUtils.format(new Date(),DateUtils.Y_M_D_H_M);
        //手机号发生改变
        boolean change=managerInfo.getUsername().equals(editVO.getUsername());
        //手机号发生改变
        if(!change){
            boolean result=smsService.checkVerificationCode(editVO.getUsername(),editVO.getPhoneCode());
            //验证码不正确
            if(!result){
                return ResponseResult.buildResponseResult(ResCodeEnum.VALIDATE_CODE_ERROR);
            }
            String param=CommonUtils.getPhoneTailNum(editVO.getUsername());
            SystemNotice notice=systemNoticeService.createNotice(managerInfo.getManagerId(),
                    NoticeTemplateType.UPDATE_PHONE_NUM,param,date);
            list.add(notice);
            managerInfo.setUsername(editVO.getUsername());
        }
        //邮箱发生改变
        boolean emailChange=editVO.getEmail()!=null && this.strIsChange(managerInfo.getEmail(),editVO.getEmail());
        if(emailChange){
            boolean result=mailService.checkVerificationCode(editVO.getEmail(),editVO.getEmailCode());
            if(!result){
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_EMAIL_CODE);
            }
            String param=CommonUtils.treateMail(editVO.getEmail());
            SystemNotice notice=systemNoticeService.createNotice(managerInfo.getManagerId(),
                    NoticeTemplateType.UPDATE_EMAIL,param,date);
            list.add(notice);
            managerInfo.setEmail(editVO.getEmail());
        }
        //昵称发生改变，看是否重复
        boolean isChange=this.strIsChange(managerInfo.getNickname(),editVO.getNickname());
        if(isChange){
            PlatformManagers repeat=managersMapper.selectOne(new PlatformManagers().setNickname(editVO.getNickname()));
            if(repeat!=null){
                log.error("新增用户时录入的昵称已经存在，无法新增用户，vo={}，重复的对象是：repeat={}",editVO,repeat);
                throw new BusinessException(ResCodeEnum.ACCOUNT_NAME_REPEAT,ExceptionMessageEnum.ACCOUNT_NAME_REPEAT);
            }
        }
        //验证通过修改个人信息
        PlatformManagers platformManagers=BeanUtils.copyProperties(editVO,PlatformManagers.class).setManagerId(managerInfo.getManagerId())
                .setUpdateId(managerInfo.getManagerId()).setUpdateAt(new Date());
        Integer result=managersMapper.updateById(platformManagers);
        if(result>0){
            //增加系统通知
            SystemNotice notice=systemNoticeService.createNotice(managerInfo.getManagerId(),
                    NoticeTemplateType.USER_UPDATE_INFORMATION,date);
            list.add(notice);
            systemNoticeService.insertAll(list);
            managerInfo.setHead(editVO.getHead()).setNotes(editVO.getNotes()).setNickname(editVO.getNickname());
            //更新缓存
            this.addManagerInfoToRedis(managerInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 将管理后台的用户信息加入到缓存中
     * @Author: junyang.li
     * @Date: 17:04 2019/5/5
     * @param managerInfo : 用户信息
     * @return: void
     */
    @Override
    public void addManagerInfoToRedis(ManagerInfo managerInfo) {
        int time=Integer.valueOf(configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN, "7200", "Token 在redis在的失效时间"));
        try {
            jedisService.delKey(managerInfo.getSysToken());
            jedisService.setObject(managerInfo.getSysToken(), managerInfo, time);
        } catch (Exception e) {
            log.error("用户信息放入缓存失败，managerInfo={},time={},错误日志:{}", managerInfo, time, e);
            jedisService.setObject(managerInfo.getSysToken(), managerInfo, time);
        }
    }

    /**
     * @notes 为管理用户生成业务id
     * @Author junyang.li
     * @Date 15:37 2019/4/25
     **/
    private String createBeesrvId(){
        try {
            return cenerateIdUtils.generateOrderId(BusinessIdType.USER);
        }catch (JedisException e){
            log.error("调用缓存生成管理用户的业务id连接异常，异常信息是:{}",e);
            int count=managersMapper.selectCount(new EntityWrapper<>());
            return cenerateIdUtils.generateOrderId(BusinessIdType.USER,count);
        }
    }

    /**
     * @notes 通过token在数据库中查询用户信息
     * @Author junyang.li
     * @Date 10:48 2019/4/30
     **/
    private ManagerInfo getManagerByToken(String sysToken){
        //查询
        PlatformManagers manager = managersMapper
                .selectOne(new PlatformManagers().setSysToken(sysToken));
        //查不到则登录信息有误
        if(manager==null){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        //判断token是否过期
        if(new DateTime(manager.getExpiresIn()).isBeforeNow()){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.SYSTOKEN_OVERDUE);
        }
        ManagerInfo managerInfo=BeanUtils.copyProperties(manager,ManagerInfo.class);
        //查出该用户角色
        MRoleInfo roleInfo=managersRolesService.getUserRoles(managerInfo.getManagerId());
        return  managerInfo.setRoleInfo(roleInfo);
    }


    /**
     * @notes: 权限-用户管理 获取用户列表
     * @Author: junyang.li
     * @Date: 11:46 2019/5/9
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.user.dto.ManagerUsersDTO>>
     */
    @Override
    public ResponseResult<List<ManagerUsersDTO>> getUserList(Pagination pagination) {
        List<PlatformManagers> list=managersMapper.selectPage(pagination,
                new EntityWrapper<PlatformManagers>().orderBy("create_at",false));
        //遍历用户id
        List<Integer> managerIds=list.stream().map(PlatformManagers::getManagerId).collect(Collectors.toList());
        Map<Integer,MRoleInfo> map=managersRolesService.getRoleByManagerIds(managerIds);
        //组转数据
        List<ManagerUsersDTO> result=list.stream().map(obj->{
            MRoleInfo info=map.get(obj.getManagerId());
            StringBuilder builder=new StringBuilder(obj.getUsername()).append(ConstantsUtil.SLASH).append(obj.getEmail());
            return  new ManagerUsersDTO().setManagerId(obj.getManagerId()).setCreateAt(obj.getCreateAt())
                    .setNickname(obj.getNickname()).setStatus(obj.getStatus()).setRoleName(info==null?null:info.getRoleName())
                    .setPhoneEmail(builder.toString());
        }).collect(Collectors.toList());
        //查询角色
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
    }

    /**
     * @notes: 用户列表账户启禁用
     * @Author: junyang.li
     * @Date: 11:49 2019/5/9
     * @param managerInfo : 操作人id
     * @param managerId : 用户id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> editAccountStatus(ManagerInfo managerInfo,Integer managerId) {
        PlatformManagers manager=managersMapper.selectById(managerId);
        if(manager==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        Status status=manager.getStatus().equals(Status.TRUE.getKey())?Status.FALSE:Status.TRUE;
        PlatformManagers newManager=new PlatformManagers()
                .setManagerId(managerId)
                .setStatus(status.getKey())
                .setUpdateAt(new Date())
                .setUpdateId(managerInfo.getManagerId());
        //账号名称为空
        String name=CommonUtils.getNickName(manager.getNickname(),manager.getUsername());
        String content;
        //禁用
        if(status.equals(Status.FALSE)){
            if(manager.getSysToken()!=null){
                //如果用户Token未过期，则清除该用户缓存
                boolean result=jedisService.exists(manager.getSysToken());
                if(result){
                    //清除用户缓存
                    jedisService.delKey(manager.getSysToken());
                }
            }
            content=operatorLogService.createContent(OperatorLogType.PROHIBIT_ACCOUNT,name,
                    managerInfo.getNickname());
        }else{
            content= operatorLogService.createContent(OperatorLogType.ENABLE_ACCOUNT,name,
                    managerInfo.getNickname());
        }
        //新增通知
        operatorLogService.insetOperatorLog(managerInfo,content);
        managersMapper.updateById(newManager);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 管理员重置后台账号的密码
     * @Author: junyang.li
     * @Date: 13:59 2019/5/9
     * @param managerInfo : 操作人
     * @param managerId : 被操作人
     * @param type : 重置的方式
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> resetMemberPassword(ManagerInfo managerInfo, Integer managerId, Integer type){
        //查询该用户是否存在
        PlatformManagers manager=managersMapper.selectById(managerId);
        if(manager==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //随机生成6位密码
        String password= RandomStringUtils.randomAlphanumeric(6);
        //加密
        String encode=BCryptPassword.encode(password);
        //修改密码
        PlatformManagers newManager=new PlatformManagers().setManagerId(managerId)
                .setPassword(encode).setUpdateId(managerInfo.getManagerId())
                .setUpdateAt(new Date());
        //系统通知集合
        List<SystemNotice> list=addSystemNotice(managerInfo,manager);
        Map<String,Object> map=new HashMap<>(2);
        map.put("password",password);
        //发送短信给用户新的密码
        if(Status.FALSE.getKey().equals(type)){
            try {
                smsService.sendMessageForPrompt(manager.getUsername(),
                        NoticeTemplateType.RESET_PASSWORD_USER.getKey(),map);
                //插入日志
                String phoneNum=CommonUtils.getPhoneTailNum(manager.getUsername());
                SystemNotice notice=systemNoticeService.createNotice(manager.getManagerId(),
                        NoticeTemplateType.RESET_PASSWORD_USER,phoneNum);
                list.add(notice);
            }catch (Exception e){
                log.error("发生短信时客户端连接失败，异常信息是：{}",e);
                return ResponseResult.buildResponseResult(ResCodeEnum.PASSWORD_RESET_FAIL);
            }
        }else {
            String email=manager.getEmail();
            if(StringUtils.isEmpty(email)){
                return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_NOT_RESET_PASSWORD);
            }
            //发送邮件
            map.put("time",DateUtils.format(new Date(),DateUtils.DEFAULT));
            ResponseResult<ResCodeEnum> result=mailService.sendMail(email, FreeMarkerType.PASSWORD_EMAIL,map);
            if(!ResCodeEnum.SUCCESS.getCode().equals(result.getCode())){
                return result;
            }
            //记录邮件
            SystemNotice notice=systemNoticeService.createNotice(manager.getManagerId(),
                    NoticeTemplateType.RESET_PASSWORD_BY_EMAIL,CommonUtils.treateMail(email));
            list.add(notice);
        }
        //账号名称为空
        String name=CommonUtils.getNickName(manager.getNickname(),manager.getUsername());
        String content= operatorLogService.createContent(OperatorLogType.RESET_PASSWORD,
                managerInfo.getNickname(),name);
        operatorLogService.insetOperatorLog(managerInfo,content);
        //插入系统通知
        managersMapper.updateById(newManager);
        systemNoticeService.insertAll(list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * @notes: 管理员重置后台账号的密码创建系统通知
     * @Author: junyang.li
     * @Date: 14:33 2019/5/9
     * @param managerInfo : 操作人信息
     * @param manager : 被操作人信息
     * @return: java.util.List<com.bee.platform.user.entity.SystemNotice>
     */
    private List<SystemNotice> addSystemNotice(ManagerInfo managerInfo,PlatformManagers manager){
        //生成操作人系统通知
        SystemNotice operator=systemNoticeService.createNotice(managerInfo.getManagerId(),
                NoticeTemplateType.RESET_PASSWORD,manager.getUsername());
        List<SystemNotice> list=new ArrayList<>();
        list.add(operator);
        return list;
    }
}

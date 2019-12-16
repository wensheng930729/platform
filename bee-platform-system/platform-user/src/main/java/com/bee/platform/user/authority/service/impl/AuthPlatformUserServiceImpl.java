package com.bee.platform.user.authority.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.exceptions.ClientException;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.AuthUserRoleInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.*;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.*;
import com.bee.platform.user.authority.dao.mapper.*;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.entity.*;
import com.bee.platform.user.authority.enums.AccountType;
import com.bee.platform.user.authority.enums.AuthPlatformUserType;
import com.bee.platform.user.authority.enums.AuthRoleType;
import com.bee.platform.user.authority.enums.EnumActiveType;
import com.bee.platform.user.authority.rq.*;
import com.bee.platform.user.authority.service.*;
import com.bee.platform.user.dao.mapper.DepartmentsMapper;
import com.bee.platform.user.dao.mapper.ZPostMapper;
import com.bee.platform.user.dto.CompanyDTO;
import com.bee.platform.user.dto.UserBasicDTO;
import com.bee.platform.user.email.MailService;
import com.bee.platform.user.entity.Departments;
import com.bee.platform.user.entity.ZPost;
import com.bee.platform.user.rq.UserAuthValidateRQ;
import com.bee.platform.user.rq.UserInCompanyRQ;
import com.bee.platform.user.sms.SmsService;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.ObjectUtils;
import org.springframework.util.PathMatcher;

import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthPlatformUserServiceImpl extends ServiceImpl<AuthPlatformUserMapper, AuthPlatformUser> implements AuthPlatformUserService {

    @Autowired
    private AuthPlatformUserMapper authPlatformUserMapper;
    @Autowired
    private SmsService smsService;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private AuthPlatformUserEnterpriseService authPlatformUserEnterpriseService;
    @Autowired
    private ConfigService configService;
    @Autowired
    private RegionService regionService;
    @Autowired
    private AuthEnterpriseService authEnterpriseService;
    @Autowired
    private MailService mailService;
    @Autowired
    private AuthUserRoleMapper authUserRoleMapper;
    @Autowired
    private AuthRoleMapper authRoleMapper;
    @Autowired
    private AuthCommonFileService commonFileService;
    @Autowired
    private AuthUserRoleService authUserRoleService;
    @Autowired
    private AuthPlatformUserEnterpriseMapper authPlatformUserEnterpriseMapper;

    @Autowired
    private AuthEnterpriseRoleService enterpriseRoleService;

    @Autowired
    private AuthWhitelistService authWhitelistService;

    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private AuthEnterpriseMapper authEnterpriseMapper;

    @Autowired
    private DepartmentsMapper departmentsMapper;

    @Autowired
    private ZPostMapper zPostMapper;
    @Autowired
    private AuthRoleService authRoleService;
    @Autowired
    private AuthEnterpriseRoleMapper authEnterpriseRoleMapper;
    @Autowired
    private AuthFunctionRoleMapper authFunctionRoleMapper;
    @Autowired
    private AuthUsergroupService usergroupService;
    @Autowired
    private AuthUsergroupRoleService usergroupRoleService;

    @Override
    public AuthPlatformUser selectOne(AuthPlatformUser user) {
        return authPlatformUserMapper.selectOne(user);
    }

    /**
     * @Description 注册第一步请求验证码
     * @Param userName
     * @Date 2019/5/24 11:34
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> getRegisterValidateCode(String userName) {
        //查询该用户是否已经存在
        AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setUsername(userName).setDeleted(Status.FALSE.getKey()));
        if (Objects.nonNull(user)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PHONE_NUMBER_EXIST);
        }
        try {
            return smsService.sendMessage(userName);
        } catch (ClientException e) {
            log.info("注册发送验证码失败，异常信息是:{}", e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
    }

    /**
     * @Description 内部系统获取用户信息
     * @Param sysToken
     * @Date 2019/5/24 11:36
     * @Author xin.huang
     * @Return
     */
    @Override
    public AuthPlatformUserInfo getUserInfo(String sysToken) {
        if (StringUtils.isBlank(sysToken)) {
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.USER_NOT_LEGAL);
        }
        AuthPlatformUserInfo userInfo = getUserInfoToRedis(sysToken);
        if (userInfo == null) {
            log.info("getUserInfo()方法中登录凭证有误无法从缓存中拿到用户信息sysToken={}", sysToken);
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        //未切换企业
        if (CollectionUtils.isEmpty(userInfo.getRoleList())) {
            log.error("用户在企业下没有角色，sysToken={}。没有权限：{}", sysToken, userInfo);
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.USER_NOT_AUTHORIZE);
        }
        return userInfo;
    }

    /**
     * @Description 通过token获得用户信息
     * @Param sysToken
     * @Date 2019/5/24 11:41
     * @Author xin.huang
     * @Return
     */
    @Override
    public AuthPlatformUserInfo getSelfInfo(String sysToken) {
        if (StringUtils.isBlank(sysToken)) {
            log.info("getSelfInfo()方法中登录凭证为空无法从缓存中拿到用户信息");
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.USER_NOT_LEGAL);
        }
        AuthPlatformUserInfo userInfo = getUserInfoToRedis(sysToken);
        if (userInfo == null || userInfo.getId() == null) {
            log.info("getSelfInfo()方法中登录凭证有误无法从缓存中拿到用户信息sysToken={}。UserInfo={}", sysToken, userInfo);
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        return userInfo;
    }

    /**
     * @Description 校验验证码
     * @Param phone
     * @Param code
     * @Date 2019/5/24 11:42
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> validateCode(String account, String code) {
        boolean result = smsService.checkVerificationCode(account, code);
        if (!result) {
            return ResponseResult.buildResponseResult(ResCodeEnum.VALIDATE_CODE_ERROR);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 注册最后一部
     * @Param rq
     * @Date 2019/5/24 11:43
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> register(RegisterUserRQ rq) {
        boolean result = smsService.getCheckResult(rq.getPhone());
        if (!result) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_VALIDATE);
        }
        AuthPlatformUser oldUser = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setUsername(rq.getPhone()).setDeleted(Status.FALSE.getKey()));
        if (oldUser != null) {
            log.info("该账号已经存在，无需注册User={}", oldUser);
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NAME_EXIT);
        }
        AuthPlatformUser user = new AuthPlatformUser()
                .setUsername(rq.getPhone())
                .setPassword(BCryptPassword.encode(rq.getPassword()))
                .setEmail(rq.getEmail())
                .setActiveType(Status.FALSE.getKey())
                .setPhone(rq.getPhone())
                .setName(rq.getName())
                .setStatus(Status.TRUE.getKey())
                .setUserType(UserType.MIDDLE_USER.getKey())
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(new Date())
                .setUpdateTime(new Date());
        authPlatformUserMapper.insert(user);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 用于存在的用户请求验证码
     * @Param phone
     * @Date 2019/5/24 11:48
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> sendMessage(String phone) {
        try {
            AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                    .setUsername(phone).setDeleted(Status.FALSE.getKey()));
            if (user == null) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND);
            }
            return smsService.sendMessage(phone);
        } catch (ClientException e) {
            log.error("用户请求验证码失败，用户手机号是：{}，异常信息是：{}", phone, e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM);
    }

    /**
     * @Description 忘记密码（用户未登录）
     * @Param password
     * @Param phone
     * @Date 2019/5/24 11:50
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> resetPassword(String password, String phone) {
        this.resetUserPassword(password, phone);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 更新后修改密码
     * @Param userInfo
     * @Param newPassword
     * @Date 2019/5/24 13:38
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> updatePassword(AuthPlatformUserInfo userInfo, String newPassword) {
        resetUserPassword(newPassword, userInfo.getUsername());
        jedisService.delKey(userInfo.getSysToken());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 查询该用户的企业id
     * @Param null
     * @Date 2019/5/24 13:40
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<Integer> userInCompany(UserInCompanyRQ rq) {
        AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setUsername(rq.getUsername()).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(user) || StringUtils.isBlank(user.getUsername())) {
            log.info("查询该用户的企业数据为空，用户账号是：{}", rq.getUsername());
            return new ArrayList<>();
        }
        return authPlatformUserEnterpriseService.userInEnterprises(user.getId(), rq.getOrgIds());
    }

    /**
     * @Description 切换企业
     * @Param userInfo
     * @Param orgId
     * @Date 2019/5/24 14:04
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> switchOrg(AuthPlatformUserInfo userInfo, Integer orgId) {
        //查询用户是否是该企业成员
        AuthPlatformUserEnterprise enterpriseUser = authPlatformUserEnterpriseService.selectOne(new EntityWrapper<AuthPlatformUserEnterprise>()
                .eq("user_id", userInfo.getId())
                .eq("enterprise_id", orgId)
                .eq("deleted", Status.FALSE.getKey()));
        if (enterpriseUser == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_IN_ENTERPRISE);
        }

        //获取用户角色列表
        getUserRoles(userInfo, orgId);

        userInfo.setOrgId(orgId);
        //企业信息
        AuthPlatformUserDto userEnterpriseInfo = authPlatformUserEnterpriseService.findUserEnterpriseInfo(userInfo.getId(), orgId);
        userInfo.setOrg_name(userEnterpriseInfo.getEnterpriseName())
                .setDepartmentName(userEnterpriseInfo.getDepartmentName())
                .setPost(userEnterpriseInfo.getPostName());
        //如果该用户未被激活，则激活该用户
        if (Status.FALSE.getKey().equals(userEnterpriseInfo.getStatus())) {
            authPlatformUserEnterpriseService.updateById(new AuthPlatformUserEnterprise()
                    .setId(orgId).setStatus(Status.TRUE.getKey()).setDeleted(Status.FALSE.getKey()));
        }
        //修改user_token表中用户当前登录的企业
        authPlatformUserMapper.updateById(new AuthPlatformUser().setId(userInfo.getId()).setCurrentEnterpriseId(orgId));
        //将用户数据存入缓存中
        addToRedis(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.CHANGE_SUCCESS);
    }

    /**
     * @Description 将用户信息插入的缓存中
     * @Param userInfo
     * @Date 2019/5/24 14:04
     * @Author xin.huang
     * @Return
     */
    @Override
    public void addToRedis(AuthPlatformUserInfo userInfo) {
        //查询配置表中token的失效时间的秒数
        int expireSeconds = Integer.valueOf(configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN,
                "7200", "Token 在redis在的失效时间"));
        try {
            jedisService.delKey(userInfo.getSysToken());
            jedisService.setObject(userInfo.getSysToken(), userInfo, expireSeconds);
        } catch (Exception e) {
            log.info("redis缓存用户信息时异常，异常信息是：{}", e);
        }
    }

    @Override
    public ResponseResult<ResCodeEnum> deleteUserForDepartment(AuthPlatformUserInfo userInfo, int id) {
        return null;
    }

    /**
     * @Description 修改个人信息
     * @Param userInfo
     * @Date 2019/5/24 14:06
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> modifySelfInfo(AuthPlatformUserInfo userInfo, EditAuthPlatformUserRQ editUserRQ) {
        AuthPlatformUser user = BeanUtils.copyProperties(editUserRQ, AuthPlatformUser.class).setId(userInfo.getId())
                .setUpdateTime(new Date()).setName(editUserRQ.getName()).setNickname(editUserRQ.getName());
        //修改用户信息
        authPlatformUserMapper.updateById(user);
        //更新用户缓存的信息
        RegionDTO dto = regionService.selectRegion(editUserRQ.getRegionId());
        userInfo.setRegion(dto);
        BeanUtils.copyProperties(editUserRQ, userInfo);
        //覆盖缓存
        addToRedis(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<UserBasicDTO> findUserAndDepartment(AuthPlatformUserInfo userInfo, String name) {
        return null;
    }

    /**
     * @Description 供应链金融根据关键词查询用户
     * @Param keyWord
     * @Date 2019/5/24 14:15
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<AuthPlatformUserInfo>> getUserByKeyWord(AuthPlatformUserInfo userInfo, String keyWord, Pagination page) {
        List<AuthPlatformUser> users = authPlatformUserMapper.selectUserByKeyword(userInfo.getOrgId(), keyWord, page);
        if (CollectionUtils.isEmpty(users)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(), PageUtils.transToPage(page));
        }
        List<AuthPlatformUserInfo> list = BeanUtils.assemble(AuthPlatformUserInfo.class, users);
        //查询该用户最近一次登陆的企业信息
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(page));
    }

    /**
     * @Description 通过用户id获取用户信息
     * @Param userInfo
     * @Param userId
     * @Date 2019/5/24 14:46
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<AuthPlatformUserDto> getUserById(AuthPlatformUserInfo userInfo, Integer userId) {
        AuthPlatformUser user = authPlatformUserMapper.selectById(userId);
        if (user == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        AuthPlatformUserEnterprise enterprisesUsers = authPlatformUserEnterpriseService
                .findByUserIdAndEnterpriseId(userId, userInfo.getOrgId());
        if (enterprisesUsers == null) {
            log.info("非本企业成员,无法获得用户信息User={}", user);
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_THE_ENTERPRISE_USER);
        }
        AuthPlatformUserDto dto = BeanUtils.copyProperties(user, AuthPlatformUserDto.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @Override
    public ResponseResult<CompanyDTO> getUserInfoCompany(String username, String company) {
        return null;
    }

    /**
     * @Description 修改用户手机号或邮箱
     * @Param sysToken
     * @Param editUserAccountRQ
     * @Date 2019/5/24 14:23
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateAccount(String sysToken, EditUserAccountRQ editUserAccountRQ) {
        String account;
        //用户是否存在
        AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setId(editUserAccountRQ.getId()).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(user)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }

        if (editUserAccountRQ.getType().equals(AccountType.phone.getCode())) {
            //验证手机号
            if (!Validator.isMobile(editUserAccountRQ.getUsername())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
            }
            //是否与旧手机号相同
            if (editUserAccountRQ.getUsername().equals(user.getUsername())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.OLD_PHONE_SAME_NEW);
            }
            account = editUserAccountRQ.getUsername();
        } else {
            //验证邮箱
            if (!Validator.isEmail(editUserAccountRQ.getEmail())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ERROR);
            }
            //是否与旧邮箱相同
            if (editUserAccountRQ.getEmail().equals(user.getEmail())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.OLD_EMAIL_SAME_NEW);
            }
            account = editUserAccountRQ.getEmail();
        }

        String result = jedisService.get(ConstantsUtil.VALIDATE_RESULT + account);
        if (org.apache.commons.lang.StringUtils.isEmpty(result) || ConstantsUtil.FALSE.equals(result)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_VALIDATE);
        }
        AuthPlatformUserInfo userInfo = getSelfInfo(sysToken);
        AuthPlatformUser newUser = new AuthPlatformUser().setId(editUserAccountRQ.getId());
        if (editUserAccountRQ.getType().equals(AccountType.phone.getCode())) {
            newUser.setUsername(editUserAccountRQ.getUsername());
            userInfo.setUsername(editUserAccountRQ.getUsername());
        } else {
            newUser.setEmail(editUserAccountRQ.getEmail());
            userInfo.setEmail(editUserAccountRQ.getEmail());
        }

        //修改用户账号信息
        newUser.setUpdateTime(new Date());
        authPlatformUserMapper.updateById(newUser);
        if (editUserAccountRQ.getType().equals(AccountType.email.getCode())) {
            //短信通知
            JSONObject jsonObject = new JSONObject();
            String newEmail = StringUtil.getSpliceEmail(newUser.getEmail());
            jsonObject.put("user_email", newEmail);
            String updateTime = new Date().toString();
            jsonObject.put("time", updateTime);
            sendMessageForPrompt(user.getUsername(), NoticeTemplateType.UPDATE_USER_EMAIL.getKey(), jsonObject);
        }
        jedisService.delKey(ConstantsUtil.VALIDATE_RESULT);
        //覆盖缓存
        addToRedis(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 获取邮箱验证码
     * @Param phone
     * @Param email
     * @Date 2019/5/24 14:29
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> sendEmail(String phone, String email) {
        try {
            //判断用户是否存在
            AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                    .setUsername(phone).setDeleted(Status.FALSE.getKey()));
            if (Objects.isNull(user)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND);
            }

            //新邮箱是否与旧邮箱相同
            if (Objects.nonNull(user.getEmail()) && user.getEmail().equals(email)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.OLD_EMAIL_SAME_NEW);
            }

            //邮箱是否已被使用
            Wrapper<AuthPlatformUser> userWrapper = new EntityWrapper<AuthPlatformUser>()
                    .eq("email", email)
                    .and().ne("username", phone);
            Integer count = authPlatformUserMapper.selectCount(userWrapper);
            if (count > 0) {
                return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_REPETITION);
            }

            return mailService.sendMail(email);
        } catch (Exception e) {
            log.error("邮箱验证码发送失败，邮箱是：{}，异常信息是：{}", email, e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM);
    }

    private AuthPlatformUserInfo getUserInfoToRedis(String sysToken) {
        try {
            //获得该对象
            return jedisService.getJsonObject(sysToken, AuthPlatformUserInfo.class);
        } catch (Exception e) {
            log.info("redis缓存连接异常，异常信息是:{}", e);
            AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                    .setSysToken(sysToken).setDeleted(Status.FALSE.getKey()));
            if (Objects.isNull(user)) {
                log.info("从数据库中查询的用户信息为空。{}", user);
                return null;
            }
            return BeanUtils.copyProperties(user, AuthPlatformUserInfo.class).setExpiresIn(user.getExpiresIn())
                    .setSysToken(user.getSysToken()).setOrgId(user.getCurrentEnterpriseId());
        }
    }

    private AuthPlatformUser resetUserPassword(String password, String phone) {
        //缓存校验结果
        boolean result = smsService.getCheckResult(phone);
        if (!result) {
            throw new BusinessException(ResCodeEnum.NOT_VALIDATE, ExceptionMessageEnum.NOT_VALIDATE);
        }
        //用户是否存在
        AuthPlatformUser user = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setUsername(phone).setDeleted(Status.FALSE.getKey()));
        //数据有误
        if (Objects.isNull(user)) {
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NOT_EXIST);
        }
        //是否与旧密码相同
        if (BCryptPassword.matches(password, user.getPassword())) {
            throw new BusinessException(ResCodeEnum.OLD_PASSWORD_SAME_NEW, ExceptionMessageEnum.OLD_PASSWORD_SAME_NEW);
        }
        AuthPlatformUser newUser = new AuthPlatformUser().setId(user.getId())
                .setPassword(BCryptPassword.encode(password)).setUpdateTime(new Date());
        //修改新密码
        authPlatformUserMapper.updateById(newUser);
        jedisService.delKey(ConstantsUtil.VALIDATE_RESULT);
        return user;
    }

    private ResponseResult<List<AuthPlatformUserInfo>> slectUserInfo(List<AuthPlatformUserInfo> list) {
        List<String> userNames = list.stream().map(AuthPlatformUserInfo::getUsername).collect(Collectors.toList());
        //查询用户相关信息
        List<AuthPlatformUser> authPlatformUsers = authPlatformUserMapper
                .selectList(new EntityWrapper<AuthPlatformUser>()
                        .in("username", userNames)
                        .eq("deleted", Status.FALSE.getKey()));
        //如果没有登录信息
        if (CollectionUtils.isEmpty(authPlatformUsers)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
        }
        List<Integer> orgIds = new ArrayList<>();
        Map<String, Integer> map = new HashMap<>(16);
        authPlatformUsers.forEach(obj -> {
            if (obj.getCurrentEnterpriseId() != null) {
                orgIds.add(obj.getCurrentEnterpriseId());
                map.put(obj.getUsername(), obj.getCurrentEnterpriseId());
            }
        });
        //如果没有企的登录信息
        if (CollectionUtils.isEmpty(orgIds)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
        }
        //查询对应的企业名称
        List<AuthEnterprise> enters = authEnterpriseService.selectList(new EntityWrapper<AuthEnterprise>()
                .in("id", orgIds)
                .eq("deleted", Status.FALSE.getKey()));
        //组装登录信息
        list.forEach(obj -> {
            Integer orgId = map.get(obj.getUsername());
            if (orgId != null) {
                for (AuthEnterprise item : enters) {
                    if (orgId.equals(item.getId())) {
                        obj.setOrg_name(item.getName()).setOrgId(orgId);
                        break;
                    }
                }
            }
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    /**
     * @Description 发送提示短信
     * @Param phone
     * @Param key
     * @Param jsonObject
     * @Return
     * @Date 2019/5/13 16:45
     * @Author xin.huang
     */
    private void sendMessageForPrompt(String phone, Integer key, JSONObject jsonObject) {
        try {
            smsService.sendMessageForPrompt(phone, key, jsonObject.toJSONString());
        } catch (ClientException e) {
            log.error("手机提示消息发送失败，手机号是:{}。错误信息是:{}", phone, e);
        }
    }

    /**
     * 添加后台用户
     *
     * @param userInfo
     * @param authPlatformUserAfterRQ
     * @return
     * @throws Exception
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> add(AuthPlatformUserInfo userInfo,
                                           AuthPlatformUserAfterRQ authPlatformUserAfterRQ) throws Exception {
        String userName = authPlatformUserAfterRQ.getPhone();
        AuthUserRole authUserRole = new AuthUserRole();
        Integer userId = null;
        boolean isNeedAddRole = false;
        AuthPlatformUser platformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setUsername(userName));
        if (Objects.nonNull(platformUser)) {
            Integer curType = platformUser.getUserType();
            // 若用户已存在，判断该用户是否是中台用户，若是，则将用户类型设置为中后台用户
            if (UserType.BACKGROUND_USER.getKey().equals(curType)
                    || UserType.MIDDLE_BACKGROUND_USER.getKey().equals(curType)) {
                log.info("用户已经是后台用户");
                throw new BusinessException(ResCodeEnum.USER_EXISTS, ExceptionMessageEnum.USER_ALREADY_ADMIN);
            } else {
                userId = platformUser.getId();
                if (authPlatformUserMapper.updateById(
                        new AuthPlatformUser().setName(authPlatformUserAfterRQ.getName()).setId(userId).setNickname(authPlatformUserAfterRQ.getName()).setUserType(UserType.MIDDLE_BACKGROUND_USER.getKey())) <= 0) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.USER_UPDATE_FAILED);
                } else {
                    isNeedAddRole = true;
                }
            }

        } else {
            AuthPlatformUser authPlatformUser =
                    BeanUtils.copyProperties(authPlatformUserAfterRQ, AuthPlatformUser.class);
            authPlatformUser.setActiveType(EnumActiveType.ADD.getCode());
            authPlatformUser.setUsername(userName);
            authPlatformUser.setUserType(UserType.BACKGROUND_USER.getKey());
            authPlatformUser.setName(StringUtils.isBlank(authPlatformUserAfterRQ.getName())
                    ? CommonUtils.disposePhoneNum(userName) : authPlatformUserAfterRQ.getName());
            authPlatformUser.setNickname(authPlatformUser.getName());

            authPlatformUser.setPassword(BCryptPassword.encode("123456"));
            authPlatformUser.setCreateTime(new Date());
            authPlatformUser.setUpdateUser(userInfo.getId());
            if (authPlatformUserMapper.insert(authPlatformUser) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.USER_SAVE_FAILED);
            } else {
                userId = authPlatformUser.getId();
                isNeedAddRole = true;
            }
        }

        // 新增后台用户，添加角色信息
        if (isNeedAddRole) {
            AuthRole role = authRoleService.selectOne(new EntityWrapper<AuthRole>()
                    .eq("role_type", EnumRoleType.SUPER_ADMIN.getCode()).eq("deleted", Status.FALSE.getKey()));
            if (Objects.isNull(role)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }

            if (authUserRoleMapper.selectCount(new EntityWrapper<AuthUserRole>(new AuthUserRole().setUserId(userId).setRoleId(role.getId()).setDeleted(Status.FALSE.getKey()))) == 0) {

                // 往角色用户表添加用户id和角色表id以及角色
                authUserRole.setUserId(userId).setRoleId(role.getId()).setRoleType(role.getRoleType())
                        .setCreateTime(new Date()).setCreateUser(userInfo.getId()).setEnterpriseId(userInfo.getOrgId())
                        .setStatus(Status.TRUE.getKey()).setUpdateTime(new Date()).setLevel(role.getLevel()).setPid(0);
                if (authUserRoleMapper.insert(authUserRole) <= 0) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.USER_ROLR_SAVE_FAILED);
                }
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * 编辑后台用户
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> update(AuthPlatformUserInfo userInfo, AuthPlatformUserAfterUpdateRQ authPlatformUserAfterUpdateRQ) {
//    	String sysToken = userInfo.getSysToken();
//    	Integer id = authPlatformUserAfterUpdataRQ.getId();
        List<String> roleTypeList = new ArrayList<>();
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setId(authPlatformUserAfterUpdateRQ.getId())
                .setDeleted(Status.FALSE.getKey()));
        if (Status.TRUE.getKey().equals(authPlatformUser.getDeleted())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        BeanUtils.copyProperties(authPlatformUserAfterUpdateRQ, authPlatformUser);
        authPlatformUser.setUpdateUser(userInfo.getId()).setNickname(authPlatformUserAfterUpdateRQ.getName());
        authPlatformUserMapper.updateById(authPlatformUser);
        //AuthUserRole authUserRole = authUserRoleMapper.selectOne(new AuthUserRole().setUserId(authPlatformUserAfterUpdateRQ.getId()).setEnterpriseId(authPlatformUserAfterUpdateRQ.getEnterpriseId()));
        //查询操作用户与角色关联信息
        List<AuthUserRole> authUserRoles = authUserRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                .eq("user_id", userInfo.getId())
                .eq("role_id", authPlatformUserAfterUpdateRQ.getEnterpriseId()));
        //查询操作用户的角色集合
        authUserRoles.stream().forEach(e -> {
            AuthRole authRole = authRoleMapper.selectOne(new AuthRole().setId(e.getRoleId()));
            roleTypeList.add(authRole.getRoleType());
        });
        //操作用户的角色包含超级管理员才能操作
        if (!roleTypeList.contains(EnumRoleType.SUPER_ADMIN.getCode()) || !AuthPlatformUserType.SUPER_ADMIN.getCode().equals(authPlatformUserAfterUpdateRQ.getRoleType())) {
            //权限配置
            Integer operationType = OperationType.UPDATE.getKey();
            if (CollectionUtils.isEmpty(authPlatformUserAfterUpdateRQ.getDepartmentAndPostList())) {
                operationType = OperationType.DELEETE.getKey();
            }
//            insertResource(authPlatformUserAfterUpdateRQ.getAuthUserRoleRQ(), authPlatformUser.getId(), userInfo.getId());
        }
        //是管理员是就要在中间表插入一条数据
        authUserRoleMapper.delete(new EntityWrapper<>(new AuthUserRole().setUserId(authPlatformUserAfterUpdateRQ.getId())));
        if (AuthPlatformUserType.SUPER_ADMIN.getCode().equals(authPlatformUserAfterUpdateRQ.getRoleType())) {
            AuthRole role = authRoleService.selectOne(new EntityWrapper<AuthRole>()
                    .eq("role_type", EnumRoleType.SUPER_ADMIN.getCode())
                    .eq("deleted", Status.FALSE.getKey()));

            AuthUserRole authUserRole = new AuthUserRole()
                    .setUserId(authPlatformUserAfterUpdateRQ.getId())
                    .setRoleId(role.getId())
                    .setPid(0)
                    .setCreateTime(new Date())
                    .setEnterpriseId(authPlatformUserAfterUpdateRQ.getEnterpriseId())
                    .setStatus(1)
                    .setLevel(0)
                    .setRoleType(role.getRoleType())
                    .setOrderNum(0)
                    .setCreateUser(userInfo.getId())
                    .setUpdateTime(new Date())
                    .setDeleted(0);
            authUserRoleMapper.insert(authUserRole);

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 编辑后台用户启用禁用
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateById(AuthPlatformUserInfo userInfo, int status, int id) {
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(id));
        if (Objects.isNull(authPlatformUser)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        } else {
            authPlatformUser.setStatus(status).setUpdateUser(userInfo.getId()).setDeleted(Status.FALSE.getKey());
            authPlatformUserMapper.updateById(authPlatformUser);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);

    }

    /**
     * 后台用户查询
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<List<AuthPlatformUserDto>> query(AuthPlatformUserInfo userInfo, AuthPlatformUserAfterSelectRQ authPlatformUserAfterSelectRQ, Pagination pagination) {
        List<AuthPlatformUserModifyDto> authPlatformUserList = new ArrayList<>();
        List<AuthPlatformUserDto> list = new ArrayList<>();
        AuthPlatformUserDto authPlatformUserDto;
        Map<String, Object> paramMap = new HashMap<>(16);
        Integer roleType = null;
        //AuthPlatformUserAfterDTO authPlatformUserAfterDTO = new AuthPlatformUserAfterDTO();
        // 查询整张表中的用户
        if (Objects.isNull(authPlatformUserAfterSelectRQ)) {
            // 查询整张表中的用户
            authPlatformUserList = authPlatformUserMapper.selectMap(pagination);
            for (AuthPlatformUserModifyDto authPlatformUserModifyDto : authPlatformUserList) {
                authPlatformUserDto = new AuthPlatformUserDto();
                BeanUtils.copyProperties(authPlatformUserModifyDto, authPlatformUserDto);
                list.add(authPlatformUserDto);
            }
            if (CollectionUtils.isEmpty(authPlatformUserList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND, Lists.newArrayList(), PageUtils.transToPage(pagination));
            }
        } else {
            if (!StringUtils.isEmpty(authPlatformUserAfterSelectRQ.getUsernameOrPhoneOrEmail())) {
                paramMap.put("usernameOrPhoneOrEmail", authPlatformUserAfterSelectRQ.getUsernameOrPhoneOrEmail());
            }
            if (null != (authPlatformUserAfterSelectRQ.getStatus())) {
                paramMap.put("status", authPlatformUserAfterSelectRQ.getStatus());
            }
            authPlatformUserList = authPlatformUserMapper.selectByMap(paramMap, pagination);
            for (AuthPlatformUserModifyDto authPlatformUserModifyDto : authPlatformUserList) {
                authPlatformUserDto = new AuthPlatformUserDto();
                BeanUtils.copyProperties(authPlatformUserModifyDto, authPlatformUserDto);
                list.add(authPlatformUserDto);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));

    }

    @Override
    public ResponseResult<List<AuthPlatformUserPullDownDto>> getBackUser() {
        ArrayList<Integer> list = new ArrayList<>(2);
        list.add(UserType.BACKGROUND_USER.getKey());
        list.add(UserType.MIDDLE_BACKGROUND_USER.getKey());
        List<AuthPlatformUser> userList = this.selectList(new EntityWrapper<AuthPlatformUser>()
                .eq("status", Status.TRUE.getKey())
                .eq("deleted", Status.FALSE.getKey())
                .in("user_type", list));
        if (CollectionUtils.isEmpty(userList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                BeanUtils.assemble(AuthPlatformUserPullDownDto.class, userList));
    }

    /**
     * @param list : 用户集合
     * @notes: 批量插入用户
     * @Author: junyang.li
     * @Date: 10:51 2019/5/24
     * @return: void
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void insertAllUser(List<AuthPlatformUser> list) {
        if (!CollectionUtils.isEmpty(list)) {
            authPlatformUserMapper.insertAllUser(list);
        }
    }


    /**
     * 添加中台用户
     */


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addIn(AuthPlatformUserInfo userInfo, AuthPlatformUserINRQ authPlatformUserINRQ) {
        AuthPlatformUser authPlatformUser = new AuthPlatformUser();
        String phone = authPlatformUserINRQ.getPhone();
        // 校验手机号
        if (!Validator.isMobile(phone)) {
            log.error("中台添加用户失败，手机号格式不正确 类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        AuthUserRole authUserRole = new AuthUserRole();
        AuthPlatformUser platformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setPhone(phone));
        if (!Objects.isNull(platformUser)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PLATFORM_USER_AT_ALREADY);
        }
        BeanUtils.copyProperties(authPlatformUserINRQ, authPlatformUser);
        authPlatformUser.setNickname(authPlatformUserINRQ.getName());
        authPlatformUser.setUpdateUser(userInfo.getId());
        if (Objects.isNull(authPlatformUserINRQ.getName())) {
            authPlatformUser.setName(authPlatformUserINRQ.getUsername());
            authPlatformUser.setNickname(authPlatformUserINRQ.getUsername());
        }
        authPlatformUserMapper.insert(authPlatformUser);
        //判断角色为企业管理员
        if (AuthPlatformUserType.ENTERPRISE_ADMIN.getCode().equals(authPlatformUserINRQ.getRoleType())) {
            AuthRole role = authRoleService.selectOne(new EntityWrapper<AuthRole>()
                    .eq("role_type", EnumRoleType.ENTERPRISE_ADMIN.getCode())
                    .eq("deleted", Status.FALSE.getKey()));
//        	AuthRole role = authRoleMapper.selectOne(new AuthRole().setRoleType(authPlatformUserINRQ.getRoleType()));
            if (Objects.isNull(role)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }
            if (authRoleMapper.insert(role) != 1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
            }
            authPlatformUser.setUpdateUser(userInfo.getId());
            authPlatformUserMapper.insert(authPlatformUser);
            //往角色用户表添加用户id和角色表id以及角色
            authUserRole.setUserId(authPlatformUser.getId())
                    .setRoleId(role.getId())
                    .setRoleType(role.getRoleType())
                    .setCreateTime(new Date())
                    .setCreateUser(userInfo.getId())
                    .setEnterpriseId(userInfo.getOrgId())
                    .setStatus(1)
                    .setUpdateTime(new Date())
                    .setLevel(role.getLevel())
                    .setPid(0);
            authUserRoleMapper.insert(authUserRole);
        }

        //权限配置
//        insertResource(authPlatformUserINRQ.getUserRoleList(), authPlatformUser.getId(), userInfo.getId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);

    }

    /**
     * 编辑用户的时候要返回数据
     */
    @Override
    public ResponseResult<AuthPlatformUserDto> getAuthPlatformUserById(AuthPlatformUserInfo userInfo, Integer id) {
        AuthPlatformUserDto authPlatformUserDto = new AuthPlatformUserDto();
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(id));
        if (authPlatformUser == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PLATFORM_USER_NOT);
        }
        BeanUtils.copyProperties(authPlatformUser, authPlatformUserDto);
        authPlatformUserDto.setUpdateName(authPlatformUser.getName())
                .setEnterpriseId(authPlatformUser.getCurrentEnterpriseId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authPlatformUserDto);
    }


    /**
     * 查询中台用户列表
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<AuthPlatformUserInDTO> queryIn(AuthPlatformUserInfo userInfo, AuthPlatformUserSelectINRQ authPlatformUserSelectINRQ, Pagination pagination) {
        Integer roleType = null;
        ArrayList<AuthPlatformUserModifyInDto> authPlatformUserModifyInDtoList = new ArrayList<>();
        List<AuthPlatformUserInDTO> list = new ArrayList<>();
        AuthPlatformUserInDTO authPlatformUserInDTO;
        authPlatformUserModifyInDtoList = authPlatformUserMapper.queryIn(authPlatformUserSelectINRQ, pagination);
        for (AuthPlatformUserModifyInDto authPlatformUserModifyInDto : authPlatformUserModifyInDtoList) {
            authPlatformUserInDTO = new AuthPlatformUserInDTO();
//        	if (EnumRoleType.ENTERPRISE_ADMIN.getCode().equals(authPlatformUserModifyInDto.getRoleType())) {
//        		roleType = AuthPlatformUserType.ENTERPRISE_ADMIN.getCode();
//        		BeanUtils.copyProperties(authPlatformUserModifyInDto, authPlatformUserInDTO);
//        		list.add(authPlatformUserInDTO.setRoleType(roleType));
//        	} else {
//        		roleType = AuthPlatformUserType.OTHER.getCode();
//        		BeanUtils.copyProperties(authPlatformUserModifyInDto, authPlatformUserInDTO);
//        		list.add(authPlatformUserInDTO.setRoleType(roleType));
//        	}
            BeanUtils.copyProperties(authPlatformUserModifyInDto, authPlatformUserInDTO);
            list.add(authPlatformUserInDTO);
        }
        return list;
    }

    /**
     * 编辑启用禁用中台用户
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateIn(AuthPlatformUserInfo userInfo, AuthPlatformUserUpdateINRQ authPlatformUserUpdateINRQ) {
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(authPlatformUserUpdateINRQ.getId()));

        List<AuthPlatformUserEnterprise> list = authPlatformUserEnterpriseMapper.selectList(new EntityWrapper<AuthPlatformUserEnterprise>(new AuthPlatformUserEnterprise()
                .setUserId(authPlatformUserUpdateINRQ.getId())));
        if (!CollectionUtils.isEmpty(list)) {
            for (AuthPlatformUserEnterprise authPlatformUserEnterprise : list) {
                authPlatformUserEnterprise.setStatus(authPlatformUserUpdateINRQ.getStatus());
                authPlatformUserEnterpriseMapper.updateById(authPlatformUserEnterprise);
            }
        }
        if (Objects.isNull(authPlatformUser)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        } else {
            authPlatformUser.setStatus(authPlatformUserUpdateINRQ.getStatus()).setUpdateUser(userInfo.getId()).setDeleted(Status.FALSE.getKey());
            authPlatformUserMapper.updateById(authPlatformUser);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @Override
    public ResponseResult<Boolean> validateRequest(UserAuthValidateRQ rq) {
        // GET_/platform-datadriver/beat
        Preconditions.checkArgument(StringUtils.isNotBlank(rq.getUri()), "uri不能为空");
        Preconditions.checkArgument(StringUtils.isNotBlank(rq.getMethod()), "method不能为空");
        String uri = rq.getUri().replaceFirst("/+", "/");
        rq.setUri(uri);
        String matchUri = new StringBuilder(rq.getMethod()).append("_").append(rq.getUri()).toString();
        // 白名单放行
        if (isExclude(matchUri)) {
            log.info("Intercept exclude whitelist request uri: {}", matchUri);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, Boolean.TRUE);
        }
        AuthPlatformUserInfo userInfo = getUserInfo(rq.getToken());
        Integer userId = userInfo.getId();
        log.info("Intercept request uri: {}", matchUri);
        List<UserInterfaceUriDTO> userInterfaceUri = authUserRoleService.getUserInterfaceUri(userId, null);

        Boolean ret = false;
        EnumRoleType roleType = getAdminRoleType(userInterfaceUri);
        // 超级管理员放行
        if (EnumRoleType.SUPER_ADMIN.equals(roleType)) {
            ret = Boolean.TRUE;
        }
        // 企业管理员,授权校验
        else if (EnumRoleType.ENTERPRISE_ADMIN.equals(roleType)) {
            ret = validateEnterpriseAdmin(userInfo.getOrgId(), null, matchUri);
        }
        // 普通用户授权校验
        else {
            ret = validateNormalUser(userInterfaceUri, matchUri);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, ret);
    }

    /**
     * 查看中台用户
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Object> queryInOne(AuthPlatformUserInfo userInfo, AuthPlatformUserOneInRQ authPlatformUserOneInRQ) {

        AuthPlatformUserOneInDTO authPlatformUserOneInDTO = authPlatformUserMapper.queryInOne(authPlatformUserOneInRQ);
        if (Objects.isNull(authPlatformUserOneInDTO)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(authPlatformUserOneInRQ.getId()));
        RegionDTO regionDTO = regionService.selectRegion(authPlatformUser.getRegionId());
        StringBuilder sb = new StringBuilder();
        sb.append(regionDTO.getCity());
        sb.append(regionDTO.getProvince());
        authPlatformUserOneInDTO.setRegionName(sb.toString());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authPlatformUserOneInDTO);

    }


    /**
     * 编辑中台用户
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateAuthPlatformUserOneIn(AuthPlatformUserInfo userInfo, AuthPlatformUserAfterUpdateRQ authPlatformUserAfterUpdateRQ) {
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(authPlatformUserAfterUpdateRQ.getId()));
        if (authPlatformUser.getDeleted().equals(Status.TRUE.getKey())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //中台用户才能编辑处理
        if (authPlatformUser.getUserType().equals(UserType.MIDDLE_USER.getKey())) {
            List<String> roleTypeList = new ArrayList<>();
            //查询操作用户与角色关联信息
            List<AuthUserRole> authUserRoles = authUserRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                    .eq("user_id", userInfo.getId()));
            //查询操作用户的角色集合
            authUserRoles.stream().forEach(e -> {
                AuthRole authRole = authRoleMapper.selectOne(new AuthRole().setId(e.getRoleId()));
                roleTypeList.add(authRole.getRoleType());
            });
            //操作用户的角色包含超级管理员才能操作
            if (roleTypeList.contains(EnumRoleType.SUPER_ADMIN.getCode())) {
                BeanUtils.copyProperties(authPlatformUserAfterUpdateRQ, authPlatformUser);
                authPlatformUser.setUpdateUser(userInfo.getId());
                authPlatformUserMapper.updateById(authPlatformUser);
            } else {
                return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_ADMIN);
            }
            //处理用户权限信息的企业id
//            List<UserRelationRoleRQ> userRelationRoleRQS = authPlatformUserAfterUpdateRQ.getAuthUserRoleRQ();
//            userRelationRoleRQS.stream().forEach(userRelationRoleRQ -> {
//                userRelationRoleRQ.setEnterpriseId(authPlatformUserAfterUpdateRQ.getEnterpriseId());
//            });
//            //权限配置
//            insertResource(userRelationRoleRQS, authPlatformUser.getId(), userInfo.getId());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * 获取用户所属管理员角色,不是管理员则返回null
     *
     * @return
     */
    private EnumRoleType getAdminRoleType(List<UserInterfaceUriDTO> userInterfaceUri) {
        for (UserInterfaceUriDTO dto : userInterfaceUri) {
            if (EnumRoleType.SUPER_ADMIN.getCode().equals(dto.getRoleType())) {
                return EnumRoleType.SUPER_ADMIN;
            }
            if (EnumRoleType.ENTERPRISE_ADMIN.getCode().equals(dto.getRoleType())) {
                return EnumRoleType.ENTERPRISE_ADMIN;
            }
        }
        return null;
    }

    /**
     * 校验普通用户是否有权访问该matchUri
     *
     * @param uris
     * @param matchUri
     * @return
     */
    private boolean validateNormalUser(List<UserInterfaceUriDTO> uris, String matchUri) {
        for (UserInterfaceUriDTO dto : uris) {
            String uriPattern = dto.getUrl().replaceAll("\\{\\*\\}", "\\\\w+");
            if (Pattern.matches(new StringBuilder(dto.getType()).append(uriPattern).toString(), matchUri)) {
                return true;
            }
        }
        return false;

    }

    /**
     * 校验企业管理员是否有权访问该matchUri
     *
     * @param enterpriseId
     * @param platform
     * @param matchUri
     * @return
     */
    private boolean validateEnterpriseAdmin(Integer enterpriseId, String platform, String matchUri) {
        List<EnterpriseUrlDTO> enterpriseInterfaceUri = enterpriseRoleService.getEnterpriseInterfaceUri(enterpriseId, platform);
        for (EnterpriseUrlDTO dto : enterpriseInterfaceUri) {
            String uriPattern = dto.getUrl().replaceAll("\\{\\*\\}", "\\\\w+");
            if (Pattern.matches(new StringBuilder(dto.getType()).append(uriPattern).toString(), matchUri)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 白名单放行
     *
     * @param uri
     * @return
     */
    private boolean isExclude(String uri) {
        List<AuthWhitelist> excludeUris = authWhitelistService.queryByPlatform();
        if (null == excludeUris || excludeUris.size() < 1) {
            return false;
        }
        PathMatcher matcher = new AntPathMatcher();
        for (AuthWhitelist excludeUri : excludeUris) {
            if (matcher.match(excludeUri.getPattern(), uri)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @Description 企业用户权限配置
     * @Param userRoleList
     * @Param userId
     * @Param createUserId
     * @Date 2019/5/29 15:14
     * @Author xin.huang
     * @Return
     */
    private void insertResource(List<UserRelationRoleRQ> userRoleList, Integer userId, Integer createUserId) {
        if (CollectionUtils.isEmpty(userRoleList)) {
            return;
        }
        userRoleList.forEach(role -> {
            role.setUserId(userId).setCreateUser(createUserId);
        });
//        authUserRoleService.assignPermissions(userId, userRoleList);
    }


    /**
     * 编辑中台用户的时候要返回数据
     */
    @Override
    public ResponseResult<AuthPlatformUserDto> getAuthPlatformUserInById(AuthPlatformUserInfo userInfo, Integer id) {
        AuthPlatformUserDto authPlatformUserDto = new AuthPlatformUserDto();
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(id));
        ArrayList<Object> enterpriseIdList = new ArrayList<>();
        ArrayList<EdpNameDTO> edpNameDTOS = new ArrayList<>();
        if (authPlatformUser == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PLATFORM_USER_NOT);
        }
        List<AuthPlatformUserEnterprise> list = authPlatformUserEnterpriseMapper.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setUserId(id)
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())));
        if (CollectionUtils.isEmpty(list)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        for (AuthPlatformUserEnterprise authPlatformUserEnterprise : list) {
            EdpNameDTO edpNameDTO = new EdpNameDTO();
            Integer enterpriseId = authPlatformUserEnterprise.getEnterpriseId();
            AuthEnterprise authEnterprise = authEnterpriseMapper.selectOne(new AuthEnterprise().setId(enterpriseId));
            if (Objects.isNull(authEnterprise)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
            }
            edpNameDTO.setEnterpriseSimpleName(authEnterprise.getName());
            Integer departmentsId = authPlatformUserEnterprise.getDepartmentsId();
            if (!ObjectUtils.isEmpty(departmentsId)) {
                Departments departments = departmentsMapper.selectOne(new Departments().setId(departmentsId));
                if (Objects.isNull(departments)) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
                }
                edpNameDTO.setDepartmentName(departments.getName());
            }
            Integer postId = authPlatformUserEnterprise.getPostId();
            if (!ObjectUtils.isEmpty(postId)) {
                ZPost zPost = zPostMapper.selectOne(new ZPost().setId(postId));
                if (Objects.isNull(zPost)) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
                }
                edpNameDTO.setPostName(zPost.getName());
            }
            edpNameDTOS.add(edpNameDTO);
            enterpriseIdList.add(enterpriseId);

        }

        BeanUtils.copyProperties(authPlatformUser, authPlatformUserDto);

        authPlatformUserDto.setEnterpriseIds(enterpriseIdList)
                .setUpdateName(authPlatformUser.getName())
                .setEdpNameDTOS(edpNameDTOS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authPlatformUserDto);
    }

    /**
     * 判断用户是否是企业管理员
     */
    @Override
    public Boolean isManager(Integer orgId, Integer userId, String roleType) {
        List<AuthUserRole> userRoles = authUserRoleService.selectList(new EntityWrapper<>(new AuthUserRole()
                .setUserId(userId)
                .setEnterpriseId(orgId)
                .setRoleType(roleType)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        if (CollectionUtils.isEmpty(userRoles)) {
            return false;
        }
        return true;
    }

    /**
     * @Description 根据用户账号查询用户所拥有的角色
     * @Param username
     * @Date 2019/6/5 14:20
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<AuthPlatformUserInfo> getAuthUserRoles(String username, Integer orgId, String sysType) {
        AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().
                setUsername(username).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(authPlatformUser)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        org.springframework.beans.BeanUtils.copyProperties(authPlatformUser, userInfo);
        
        /*List<AuthUserRole> authUserRoles = authUserRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                .eq("",sysType)
                .eq("user_id", authPlatformUser.getId())
                .eq("role_type", AuthRoleType.ENTERPRISE_ADMIN.getDesc())
                .eq("deleted", Status.FALSE.getKey()));
                .eq("deleted", Status.FALSE.getKey()));*/
        Map<String, Object> map = new HashMap<>(16);
        map.put("id", userInfo.getId());
        map.put("sysType", sysType);
        map.put("enterpriseId", orgId);
        List<AuthUserRole> authUserRoles = authUserRoleMapper.selectAuthUserRole(map);
        if (CollectionUtils.isNotEmpty(authUserRoles)) {
            //查询用户角色列表
            List<Integer> roleIds = new ArrayList<Integer>();
            for (AuthUserRole userRole : authUserRoles) {
                roleIds.add(userRole.getRoleId());
            }
            List<AuthUserRoleInfo> authUserRoleList = BeanUtils.assemble(AuthUserRoleInfo.class, authRoleMapper.selectBatchIds(roleIds));
            userInfo.setRoleList(authUserRoleList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }

    @Override
    public ResponseResult<List<AuthPlatformUserPullDownDto>> getAllCompanyUserById(Integer orgId) {
        List<AuthPlatformUserPullDownDto> users = authPlatformUserMapper.getAllCompanyUserById(orgId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, users);
    }

    /**
     * 获取用户所在用户组中的角色列表
     *
     * @param userId
     * @param enterpriseId
     */
    private List<AuthUsergroupRole> getUserGroupRoles(Integer userId, Integer enterpriseId) {
        List<AuthUsergroup> usergroups = usergroupService.getByEnterpriseAndUserId(enterpriseId, userId);
        if (CollectionUtils.isEmpty(usergroups)) {
            return Collections.emptyList();
        }
        List<Integer> groupIds = usergroups.stream().map(usergroup -> usergroup.getId()).collect(Collectors.toList());
        List<AuthUsergroupRole> usergroupRoles = usergroupRoleService.selectList(new EntityWrapper<AuthUsergroupRole>()
                .eq("enterprise_id", enterpriseId)
                .and()
                .in("usergroup_id", groupIds)
                .and()
                .eq("deleted", Status.FALSE.getKey()));
        return usergroupRoles;
    }

    /**
     * 获取用户角色列表
     *
     * @param userInfo
     * @param orgId
     */
    private void getUserRoles(AuthPlatformUserInfo userInfo, Integer orgId) {
        //存放角色id
        Set<Integer> roleIds = new HashSet<>();
        //查询用户在当前企业中的角色
        List<AuthUserRole> authUserRoles = authUserRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                .eq("user_id", userInfo.getId())
                .eq("enterprise_id", orgId)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isNotEmpty(authUserRoles)) {
            authUserRoles.forEach(userRole -> {
                roleIds.add(userRole.getRoleId());
            });
        }
        //查询用户所在用户组中的角色
        List<AuthUsergroupRole> userGroupRoles = getUserGroupRoles(userInfo.getId(), orgId);
        if (CollectionUtils.isNotEmpty(userGroupRoles)) {
            userGroupRoles.forEach(groupRole -> {
                roleIds.add(groupRole.getRoleId());
            });
        }
        //角色不为空
        if (CollectionUtils.isNotEmpty(roleIds)) {
            List<AuthRole> authRoles = authRoleMapper.selectBatchIds(roleIds);
            if (CollectionUtils.isEmpty(authRoles)) {
                throw new BusinessException(ResCodeEnum.NOT_FOUND_USER_ROLE, ExceptionMessageEnum.NOT_FOUND_USER_ROLE);
            }
            //是否是企业管理员
            boolean managerFlag = false;
            for (AuthRole authRole : authRoles) {
                if (EnumRoleType.ENTERPRISE_ADMIN.getCode().equals(authRole.getRoleType())) {
                    userInfo.setManagerType(AuthRoleType.ENTERPRISE_ADMIN.getCode());
                    managerFlag = true;
                    break;
                }
            }
            //若是企业管理员，则查询企业管理员所拥有的基础角色
            if (managerFlag) {
                List<AuthEnterpriseRole> authEnterpriseRoles = authEnterpriseRoleMapper
                        .selectList(new EntityWrapper<>(new AuthEnterpriseRole()
                                .setEnterpriseId(orgId)
                                .setDeleted(Status.FALSE.getKey())));
                if (!CollectionUtils.isEmpty(authEnterpriseRoles)) {
                    //存放二级权限
                    List<Integer> twoRoleIds = new ArrayList<>();
                    for (AuthEnterpriseRole enterpriseRole : authEnterpriseRoles) {
                        roleIds.add(enterpriseRole.getRoleId());
                        if (enterpriseRole.getRoleType().equals(EnumRoleType.FUNCTION_TWO.getCode())) {
                            twoRoleIds.add(enterpriseRole.getRoleId());
                        }
                    }
                    //查询二级权限下的权限
                    if (!CollectionUtils.isEmpty(twoRoleIds)) {
                        List<AuthFunctionRole> authFunctionRoles = authFunctionRoleMapper
                                .selectList(new EntityWrapper<AuthFunctionRole>()
                                        .in("pid", twoRoleIds).and()
                                        .eq("deleted", Status.FALSE.getKey()));
                        if (!CollectionUtils.isEmpty(authFunctionRoles)) {
                            authEnterpriseRoles.forEach(authEnterpriseRole -> {
                                roleIds.add(authEnterpriseRole.getRoleId());
                            });
                        }
                    }
                }
                //查询所有用户角色
                authRoles = authRoleMapper.selectList(new EntityWrapper<AuthRole>()
                        .in("id", roleIds).and().eq("deleted", 0));
            }
            userInfo.setRoleList(BeanUtils.assemble(AuthUserRoleInfo.class, authRoles));
        }
    }

    @Override
    public ResponseResult<List<AuthPlatformUserFeignDTO>> getMoreUserInfo(List<Integer> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<AuthPlatformUser> userList = this.selectList(new EntityWrapper<>(new AuthPlatformUser()
                .setDeleted(Status.FALSE.getKey()))
                .in("id", ids));
        List<AuthPlatformUserFeignDTO> dtoList = BeanUtils.assemble(AuthPlatformUserFeignDTO.class, userList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dtoList);
    }
}

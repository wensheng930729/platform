package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.exceptions.ClientException;
import com.baomidou.mybatisplus.enums.SqlLike;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumIsManager;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.constants.enums.EnumSearchMode;
import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.*;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.*;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.constants.enums.EnumRole;
import com.bee.platform.user.dao.mapper.UserMapper;
import com.bee.platform.user.dao.mapper.UsersDepartmentsMapper;
import com.bee.platform.user.dao.mapper.UsersRolesMapper;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.email.MailService;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.enums.AccountType;
import com.bee.platform.user.rq.*;
import com.bee.platform.user.service.*;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.CenerateIdUtils;
import com.bee.platform.user.utils.PinyinUtil;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-04
 */
@Slf4j
@Service
public class UsersServiceImpl extends ServiceImpl<UserMapper, User> implements UsersService {

    @Autowired
    private UserMapper userMapper;
    @Autowired
    private SmsService smsService;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private UsersRolesMapper usersRolesMapper;
    @Autowired
    private URolesService rolesService;
    @Autowired
    private RegionService regionService;
    @Autowired
    private EnterprisesUsersService enterprisesUsersService;
    @Autowired
    private EnterprisesService enterprisesService;
    @Autowired
    private DepartmentsService departmentsService;
    @Autowired
    private ConfigService configService;
    @Autowired
    private UserTokensService userTokensService;
    @Autowired
    private UsersDepartmentsMapper usersDepartmentsMapper;
    @Autowired
    private MailService mailService;
    @Autowired
    private CommonRegionService commonRegionService;
    @Autowired
    private SystemNoticeService systemNoticeService;
    @Autowired
    private ZPostService postService;
    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;
    @Autowired
    private OperatorLogService operatorLogService;

    @Autowired
    private AuthPlatformUserService authPlatformUserService;

    /**
     * @notes 注册第一步请求验证码
     * @Author junyang.li
     * @Date 10:43 2019/3/4
     **/
    @Override
    public ResponseResult<ResCodeEnum> getRegisterValidateCode(String userName) {
        //查询该用户是否已经存在
        User user = userMapper.selectOne(new User().setUsername(userName));
        if (user != null && user.getId() != null) {
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
     * @notes 内部系统获取用户信息
     * @Author junyang.li
     * @Date 18:39 2018/12/11
     **/
    @Override
    public UserInfo getUserInfo(String sysToken) {
        AuthPlatformUserInfo authPlatformUserInfo = authPlatformUserService.getSelfInfo(sysToken);
        if (authPlatformUserInfo == null || authPlatformUserInfo.getOrgId() == null) {
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.NOT_CHOICE_ENTERPRISE);
        }
        return BeanUtils.copyProperties(authPlatformUserInfo, UserInfo.class).setConsumerName(authPlatformUserInfo.getName())
                .setRegionid(authPlatformUserInfo.getRegionId()).setCreateAt(authPlatformUserInfo.getCreateTime())
                .setUpdateAt(authPlatformUserInfo.getUpdateTime());
    }

    /**
     * @notes 通过token获得用户信息
     * @Author junyang.li
     * @Date 18:39 2018/12/11
     **/
    @Override
    public UserInfo getSelfInfo(String sysToken) {
        AuthPlatformUserInfo authPlatformUserInfo = authPlatformUserService.getSelfInfo(sysToken);
        if (authPlatformUserInfo == null) {
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO, ExceptionMessageEnum.NOT_CHOICE_ENTERPRISE);
        }
        return BeanUtils.copyProperties(authPlatformUserInfo, UserInfo.class).setConsumerName(authPlatformUserInfo.getName())
                .setRegionid(authPlatformUserInfo.getRegionId()).setCreateAt(authPlatformUserInfo.getCreateTime())
                .setUpdateAt(authPlatformUserInfo.getUpdateTime());
    }

    /**
     * @notes 通过sysToken
     * @Author junyang.li
     * @Date 14:17 2019/3/6
     **/
    private UserInfo getUserInfoToRedis(String sysToken) {
        try {
            //获得该对象
            return jedisService.getObject(sysToken, UserInfo.class);
        } catch (Exception e) {
            log.info("redis缓存连接异常，异常信息是:{}", e);
            UserToken token = userTokensService.selectOne(new EntityWrapper<UserToken>()
                    .where("sys_token={0}", sysToken));
            if (ObjectUtils.isEmpty(token)) {
                log.info("从数据库中查询的token信息为空。{}", token);
                return null;
            }
            User user = userMapper.selectOne(new User().setUsername(token.getUsername()));
            if (user == null) {
                log.info("从数据库中查询的用户token信息为空。{}", token);
                return null;
            }
            return BeanUtils.copyProperties(user, UserInfo.class).setExpiresIn(token.getExpiresIn())
                    .setSysToken(token.getSysToken()).setOrgId(token.getNowCompany());
        }
    }

    /**
     * @notes 校验验证码
     * @Author junyang.li
     * @Date 14:09 2019/3/4
     **/
    @Override
    public ResponseResult<ResCodeEnum> validateCode(String account, String code) {
        boolean result = smsService.checkVerificationCode(account, code);
        if (!result) {
            return ResponseResult.buildResponseResult(ResCodeEnum.VALIDATE_CODE_ERROR);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 注册最后一部
     * @Author junyang.li
     * @Date 15:27 2019/3/4
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> register(RegisterRQ rq) {
        boolean result = smsService.getCheckResult(rq.getPhone());
        if (!result) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_VALIDATE);
        }
        User oldUser = userMapper.selectOne(new User().setUsername(rq.getPhone()));
        if (oldUser != null) {
            log.info("该账号已经存在，无需注册User={}", oldUser);
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NAME_EXIT);
        }
        User user = new User().setUuid(UUID.randomUUID().toString().replaceAll("-", ""))
                .setUsername(rq.getPhone())
                .setPassword(BCryptPassword.encode(rq.getPassword()))
                .setEmail(rq.getEmail())
                .setPhone(rq.getPhone())
                .setNickname(rq.getNickname())
                .setCreateAt(new Date()).setUpdateAt(new Date());
        userMapper.insert(user);
        userMapper.updateById(new User()
                .setId(user.getId()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 查询单个用户
     * @Author junyang.li
     * @Date 17:08 2019/3/4
     **/
    @Override
    public User selectOne(User user) {
        return userMapper.selectOne(user);
    }

    /**
     * @notes 用于存在的用户请求验证码
     * @Author junyang.li
     * @Date 11:00 2019/3/5
     **/
    @Override
    public ResponseResult<ResCodeEnum> sendMessage(String phone) {
        try {
            User user = userMapper.selectOne(new User().setUsername(phone));
            if (user == null || user.getId() == null) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND);
            }
            return smsService.sendMessage(phone);
        } catch (ClientException e) {
            log.error("用户请求验证码失败，用户手机号是：{}，异常信息是：{}", phone, e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM);
    }

    /**
     * @notes 忘记密码（用户未登录）
     * @Author junyang.li
     * @Date 15:21 2019/3/5
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> resetPassword(String password, String phone) {
        this.resetUserPassword(password, phone);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 更新后修改密码
     * @Author junyang.li
     * @Date 15:49 2019/3/5
     **/
    @Override
    public ResponseResult<ResCodeEnum> updatePassword(UserInfo userInfo, String newPassword) {
        resetUserPassword(newPassword, userInfo.getUsername());
        jedisService.delKey(userInfo.getSysToken());
        middleSystemNoticeService.createNotice(userInfo.getId(),
                EnumMiddleNoticeTitle.title.CHANGE_PASSWORD_NOTICE.getValue(),
                MiddleNoticeContentTemplate.UPDATE_USER_PASSWORD.getKey(),
                new Date());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 查询该用户的企业id
     * @Author junyang.li
     * @Date 17:07 2019/1/18
     **/
    @Override
    public List<Integer> userInCompany(UserInCompanyRQ rq) {
        User user = userMapper.selectOne(new User().setUsername(rq.getUsername()));
        if (user == null || org.apache.commons.lang3.StringUtils.isBlank(user.getUsername())) {
            log.info("查询该用户的企业数据为空，用户账号是：{}", rq.getUsername());
            return new ArrayList<>();
        }
        return enterprisesUsersService.userInEnterprises(user.getId(), rq.getOrgIds());
    }

    /**
     * @notes 切换企业
     * @Author junyang.li
     * @Date 16:41 2019/3/5
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> switchOrg(UserInfo userInfo, Integer orgId) {
        UserRole userRole = usersRolesMapper.selectOne(new UserRole().setUserId(userInfo.getId()).setOrgId(orgId));
        if (userRole == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_IN_ENTERPRISE);
        }
        Role uRole = rolesService.selectOne(userRole.getRoleId());
        if (uRole == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USER_ROLE);
        }
        //是否是管理员
        userInfo.setOrgId(userRole.getOrgId()).setRoleId(uRole.getId()).setRoleName(uRole.getName())
                .setManage(RoleType.isManage(uRole.getId()));
        //企业用户信息
        EnterprisesUsers enterprisesUsers = enterprisesUsersService.selectOne(new EntityWrapper<EnterprisesUsers>()
                .where("user_id={0} and enterprise_id={1}", userInfo.getId(), userInfo.getOrgId()));

        //查询企业
        Enterprises enterprise = enterprisesService.selectOne(new EntityWrapper<Enterprises>()
                .where("id={0}", userInfo.getOrgId()));
        //查询部门
        DepartmentInfo department = departmentsService.selectByUserIdAndOrgId(userInfo.getId(), userInfo.getOrgId());
        userInfo.setOrg_name(enterprise.getName()).setDepartment(department)
                .setPost(enterprisesUsers.getPost());
        //如果该用户未被激活，则激活该用户
        if (Status.FALSE.getKey().equals(enterprisesUsers.getIsActive())) {
            enterprisesUsersService.updateById(new EnterprisesUsers().setId(enterprisesUsers.getId()).setIsActive(Status.FALSE.getKey()));
        }
        //修改user_token表中用户当前登录的企业
        userTokensService.updateByParam(new UserToken().setUsername(userInfo.getUsername()).setNowCompany(orgId));
        //将用户数据存入缓存中
        addToRedis(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.CHANGE_SUCCESS);
    }

    /**
     * @notes 将用户信息插入的缓存中
     * @Author junyang.li
     * @Date 11:08 2019/3/6
     **/
    @Override
    public void addToRedis(UserInfo userInfo) {
        //查询配置表中token的失效时间的秒数
        int expireSeconds = Integer.valueOf(configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN, "7200", "Token 在redis在的失效时间"));
        try {
            jedisService.delKey(userInfo.getSysToken());
            jedisService.setJsonObject(userInfo.getSysToken(), userInfo, expireSeconds);
        } catch (Exception e) {
            log.info("redis缓存用户信息时异常，异常信息是：{}", e);
        }
    }

    /**
     * @notes 删除员工
     * @Author junyang.li
     * @Date 14:28 2019/3/6
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteUserForDepartment(UserInfo userInfo, int id) {
        UserRole userRole = usersRolesMapper.selectOne(new UserRole().setOrgId(userInfo.getOrgId()).setUserId(id));
        //未查询到用户
        if (userRole == null || userRole.getId() == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_ENTERPRISES_NOT_FOUND);
        }
        //超级管理员不能删除
        if (RoleType.SUPER.getCode().equals(userRole.getRoleId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CANNOT_DELETE_SUPER);
        }
        //查询该企业中的部门
        List<Departments> departments = departmentsService.selectList(new EntityWrapper<Departments>()
                .where("org_id={0}", userRole.getOrgId()));
        if (!CollectionUtils.isEmpty(departments)) {
            List<Integer> departmentId = departments.stream().map(Departments::getId).collect(Collectors.toList());
            usersDepartmentsMapper.deleteUserInDepart(id, departmentId);
        }
        //删除企业中的该用户
        enterprisesUsersService.delete(new EntityWrapper<EnterprisesUsers>()
                .where("user_id={} and enterprise_id={1}", userRole.getUserId(), userRole.getOrgId()));
        //删除角色中的用户
        usersRolesMapper.deleteById(userRole.getId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 修改个人信息
     * @Author junyang.li
     * @Date 11:28 2019/3/14
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> modifySelfInfo(UserInfo userInfo, EditUserRQ editUserRQ) {
        User user = BeanUtils.copyProperties(editUserRQ, User.class).setId(userInfo.getId())
                .setUpdateAt(new Date()).setNicknamePinyin(PinyinUtil.toPinyin(editUserRQ.getNickname()));
        //修改用户信息
        userMapper.updateById(user);
        //更新用户缓存的信息
        RegionDTO dto = regionService.selectRegion(editUserRQ.getRegionid());
        userInfo.setRegion(dto).setNickname(editUserRQ.getNickname()).setRegionid(editUserRQ.getRegionid())
                .setAddress(editUserRQ.getAddress()).setQq(editUserRQ.getQq()).setFixtel(editUserRQ.getFixtel())
                .setHead(editUserRQ.getHead()).setConsumerName(editUserRQ.getConsumerName());
        if (!StringUtils.isEmpty(editUserRQ.getEmail())) {
            userInfo.setEmail(editUserRQ.getEmail());
        }
        //覆盖缓存
        addToRedis(userInfo);

        //保存修改完成提示消息
        middleSystemNoticeService.createNotice(user.getId(),
                EnumMiddleNoticeTitle.title.CHANGE_BASIC_DATA_NOTICE.getValue(),
                MiddleNoticeContentTemplate.UPDATE_USER_INFO_SUCCESS.getKey(),
                new Date());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 修改头像
     * @Author junyang.li
     * @Date 12:29 2019/3/14
     **/
    @Override
    public ResponseResult<ResCodeEnum> modifyHead(UserInfo userInfo, String head) {
        return null;
    }

    /**
     * @notes 搜索用户
     * @Author junyang.li
     * @Date 13:50 2019/3/14
     **/
    @Override
    public ResponseResult<UserBasicDTO> findUserAndDepartment(UserInfo userInfo, String name) {
        List<EnterprisesUsers> userEnter = enterprisesUsersService.selectList(new EntityWrapper<EnterprisesUsers>()
                .where("enterprise_id", userInfo.getOrgId()).and().like(true, "nickname", name, SqlLike.DEFAULT));
        List<Departments> departments = departmentsService.selectList(new EntityWrapper<Departments>()
                .where("enterprise_id", userInfo.getOrgId()).and().like(true, "nickname", name, SqlLike.DEFAULT));
        UserBasicDTO basic = new UserBasicDTO();
        if (!CollectionUtils.isEmpty(userEnter)) {
            List<UsersResultDTO> dtos = userEnter.stream().map(obj -> {
                return new UsersResultDTO().setId(obj.getUserId())
                        .setName(obj.getNickname());
            }).collect(Collectors.toList());
            basic.setUsers(dtos);
        }
        if (!CollectionUtils.isEmpty(departments)) {
            List<DepartmentResultDTO> list = departments.stream().map(obj -> {
                return new DepartmentResultDTO()
                        .setId(obj.getId()).setName(obj.getName());
            }).collect(Collectors.toList());
            basic.setDepartments(list);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, basic);
    }


    /**
     * @notes 供应链金融根据关键词查询用户
     * @Author junyang.li
     * @Date 16:05 2019/3/14
     **/
    @Override
    public ResponseResult<List<UserInfo>> getUserByKeyWord(String keyWord) {
        List<UserInfo> list = new ArrayList<>();
        if (Validator.isMobile(keyWord)) {
            User user = userMapper.selectOne(new User().setUsername(keyWord));
            //如果查询为空
            list.add(BeanUtils.copyProperties(user, UserInfo.class));
        } else {
            List<User> users = userMapper.selectList(new EntityWrapper<>(new User().setNickname(keyWord)));
            list = BeanUtils.assemble(UserInfo.class, users);
        }
        //查询该用户最近一次登陆的企业信息
        return slectUserInfo(list);
    }

    /**
     * @notes 通过用户id获取用户信息
     * @Author junyang.li
     * @Date 17:29 2019/3/21
     **/
    @Override
    public ResponseResult<UserDetailDTO> getUserById(UserInfo userInfo, Integer userId) {
        User user = userMapper.selectById(userId);
        if (user == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        EnterprisesUsers enterprisesUsers = enterprisesUsersService.findByUserIdAndEnterpriseId(userId, userInfo.getOrgId());
        if (enterprisesUsers == null) {
            log.info("非本企业成员,无法获得用户信息User={}", user);
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_THE_ENTERPRISE_USER);
        }
        UserDetailDTO dto = BeanUtils.copyProperties(user, UserDetailDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    /**
     * @notes 内部服务获取指定用户的企业信息
     * @Author junyang.li
     * @Date 16:32 2019/3/24
     **/
    @Override
    public ResponseResult<CompanyDTO> getUserInfoCompany(String username, String company) {
        User user = userMapper.selectOne(new User().setUsername(username));
        if (user == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //查询企业是否存在
        List<Enterprises> list = enterprisesService.
                selectList(new EntityWrapper<>(new Enterprises().setName(company)));
        if (CollectionUtils.isEmpty(list)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        //重复企业
        if (list.size() > 1) {
            log.error("企业{}，在数据库中重复{}", company, JSONObject.toJSONString(list));
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_REPEAT);
        }
        Enterprises enterprises = list.get(0);
        //查询用户是否在该企业中
        EnterprisesUsers enterprisesUsers = enterprisesUsersService.findByUserIdAndEnterpriseId(user.getId(), enterprises.getId());
        if (enterprisesUsers == null) {
            log.info("用户{}，未在企业{}中", username, company);
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_USER_NOT_FOUND);
        }
        CompanyDTO dto = new CompanyDTO().setCompanyId(enterprises.getId()).setCompany(enterprises.getName())
                .setLogoUrl(enterprises.getHead()).setAddress(enterprises.getAddress());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    /**
     * @notes
     * @Author junyang.li
     * @Date 16:25 2019/3/14
     **/
    private ResponseResult<List<UserInfo>> slectUserInfo(List<UserInfo> list) {
        List<String> userNames = list.stream().map(UserInfo::getUsername).collect(Collectors.toList());
        //查询用户相关信息
        List<UserToken> userTokens = userTokensService.selectList(new EntityWrapper<UserToken>()
                .in("username", userNames));
        //如果没有登录信息
        if (CollectionUtils.isEmpty(userTokens)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
        }
        List<Integer> orgIds = new ArrayList<>();
        Map<String, Integer> map = new HashMap<>(16);
        userTokens.forEach(obj -> {
            if (obj.getNowCompany() != null) {
                orgIds.add(obj.getNowCompany());
                map.put(obj.getUsername(), obj.getNowCompany());
            }
        });
        //如果没有企的登录信息
        if (CollectionUtils.isEmpty(orgIds)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
        }
        //查询对应的企业名称
        List<Enterprises> enters = enterprisesService.selectList(new EntityWrapper<Enterprises>().in("id", orgIds));
        //组装登录信息
        list.forEach(obj -> {
            Integer orgId = map.get(obj.getUsername());
            if (orgId != null) {
                for (Enterprises item : enters) {
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
     * @return
     * @Description 修改用户手机号或邮箱
     * @Author xin.huang
     * @Param userId
     * @Param editUserPhoneEmailRQ
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateAccount(String sysToken, EditUserAccountRQ editUserAccountRQ) {
        String account;
        //用户是否存在
        User user = userMapper.selectOne(new User().setId(editUserAccountRQ.getId()));
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
        if (StringUtils.isEmpty(result) || ConstantsUtil.FALSE.equals(result)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_VALIDATE);
        }

        UserInfo userInfo = getSelfInfo(sysToken);
        User newUser = new User().setId(editUserAccountRQ.getId());
        if (editUserAccountRQ.getType().equals(AccountType.phone.getCode())) {
            newUser.setUsername(editUserAccountRQ.getUsername());
            userInfo.setUsername(editUserAccountRQ.getUsername());
        } else {
            newUser.setEmail(editUserAccountRQ.getEmail());
            userInfo.setEmail(editUserAccountRQ.getEmail());
        }

        //修改用户账号信息
        newUser.setUpdateAt(new Date());
        userMapper.updateById(newUser);
        if (editUserAccountRQ.getType().equals(AccountType.email.getCode())) {
            //短信通知
            JSONObject jsonObject = new JSONObject();
            String newEmail = StringUtil.getSpliceEmail(newUser.getEmail());
            jsonObject.put("user_email", newEmail);
            String updateTime = new Date().toString();
            jsonObject.put("time", updateTime);
            sendMessageForPrompt(user.getUsername(), NoticeTemplateType.UPDATE_USER_EMAIL.getKey(), jsonObject);

            //保存提示消息
            middleSystemNoticeService.createNotice(user.getId(),
                    EnumMiddleNoticeTitle.title.CHANGE_EMAIL_NOTICE.getValue(),
                    MiddleNoticeContentTemplate.UPDATE_USER_EMAIL.getKey(),
                    newEmail, updateTime);
        } else {
            //保存提示消息
            middleSystemNoticeService.createNotice(user.getId(),
                    EnumMiddleNoticeTitle.title.CHANGE_PHONE_NOTICE.getValue(),
                    MiddleNoticeContentTemplate.UPDATE_USER_PHONE.getKey(),
                    newUser.getUsername().substring(7), new Date());
        }
        jedisService.delKey(ConstantsUtil.VALIDATE_RESULT);
        //覆盖缓存
        addToRedis(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 获取邮箱验证码
     * @Author xin.huang
     * @Param phone
     * @Param email
     **/
    @Override
    public ResponseResult<ResCodeEnum> sendEmail(String phone, String email) {
        try {
            //判断用户是否存在
            User user = userMapper.selectOne(new User().setUsername(phone));
            if (Objects.isNull(user)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ACCOUNT_NOT_FOUND);
            }

            //新邮箱是否与旧邮箱相同
            if (Objects.nonNull(user.getEmail()) && user.getEmail().equals(email)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.OLD_EMAIL_SAME_NEW);
            }

            //邮箱是否已被使用
            Wrapper<User> userWrapper = new EntityWrapper<User>()
                    .eq("email", email)
                    .and().ne("username", phone);
            Integer count = userMapper.selectCount(userWrapper);
            if (count > 0) {
                return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_REPETITION);
            }

            return mailService.sendMail(email);
        } catch (Exception e) {
            log.error("邮箱验证码发送失败，邮箱是：{}，异常信息是：{}", email, e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM);
    }

    @Override
    public ResponseResult<List<UserManagerListDTO>> getUserManagerList(UserManagerListRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Map<Integer, String> enterpriseMap = new HashMap<>(500);
        // 查询企业信息 放入map key：企业id  value：企业名字
        List<Enterprises> enterprisesList = enterprisesService.selectList(new EntityWrapper<>(new Enterprises()));
        enterprisesList.forEach(enterprise -> enterpriseMap.put(enterprise.getId(), enterprise.getName()));
        // 查询用户企业关联信息
        List<EnterprisesUsers> enterprisesUsersList = enterprisesUsersService.selectList(new EntityWrapper<>(new EnterprisesUsers()));

        // 如果是根据企业名字查询
        if (EnumSearchMode.SearchMode.enterprise.getKey().equals(rq.getMode())) {
            // 获取企业id
            List<Integer> enterpriseIdList = Lists.newArrayList();
            for (Enterprises enterprise : enterprisesList) {
                if (enterprise.getName().contains(rq.getParam())) {
                    enterpriseIdList.add(enterprise.getId());
                }
            }
            if (CollectionUtils.isEmpty(enterpriseIdList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_FIND, Lists.newArrayList(), PageUtils.transToPage(pagination));
            }
            List<EnterprisesUsers> enterpriseUsersList = enterprisesUsersService.selectList(new EntityWrapper<>(new EnterprisesUsers())
                    .in("enterprise_id", enterpriseIdList));
            // 该企业下所有 用户的id
            ArrayList<Integer> ids = new ArrayList<>();
            enterpriseUsersList.forEach(enterpriseUser -> ids.add(enterpriseUser.getUserId()));
            // 根据用户ids 查询所有符合条件的用户
            // 构建条件
            Wrapper<User> wrapper = new EntityWrapper<>(new User())
                    .in("id", ids)
                    .orderBy("create_at", false);
            if (!ObjectUtils.isEmpty(rq.getStart()) && !ObjectUtils.isEmpty(rq.getEnd())) {
                wrapper.between("create_at", rq.getStart(), rq.getEnd());
            }
            // 查询符合要求的用户
            List<User> users = userMapper.selectPage(pagination, wrapper);
            // 转换符合要求的用户
            List<UserManagerListDTO> userManagerListDTOS = BeanUtils.assemble(UserManagerListDTO.class, users);
            // 查询用户 与 企业关联信息
            getUserEnterpriseLink(userManagerListDTOS, enterprisesUsersList, enterpriseMap);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userManagerListDTOS, PageUtils.transToPage(pagination));
        }
        // 根据用户名或手机号查询
        else {
            User wrapperUser = new User();
            if (rq.getMode().equals(EnumSearchMode.SearchMode.username.getKey()) && !StringUtils.isEmpty(rq.getParam())) {
                wrapperUser.setNickname(rq.getParam());
            } else if (rq.getMode().equals(EnumSearchMode.SearchMode.phone.getKey()) && !StringUtils.isEmpty(rq.getParam())) {
                wrapperUser.setPhone(rq.getParam());
            }
            Wrapper<User> wrapper = new EntityWrapper<>(wrapperUser).orderBy("create_at", false);
            if (!ObjectUtils.isEmpty(rq.getStart())) {
                wrapper.between("create_at", rq.getStart(), rq.getEnd());
            }
            // 查询符合条件的user
            List<User> users = userMapper.selectPage(pagination, wrapper);
            // 转换符合要求的用户
            List<UserManagerListDTO> userManagerListDTOS = BeanUtils.assemble(UserManagerListDTO.class, users);
            // 查询用户 与 企业关联信息
            getUserEnterpriseLink(userManagerListDTOS, enterprisesUsersList, enterpriseMap);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userManagerListDTOS, PageUtils.transToPage(pagination));
        }
    }

    private void getUserEnterpriseLink(List<UserManagerListDTO> userManagerListDTOS,
                                       List<EnterprisesUsers> enterprisesUsersList,
                                       Map<Integer, String> enterpriseMap) {
        // 遍历每个列表项 设置参数
        userManagerListDTOS.forEach(managerListDTO -> {
            // 该用户下的所有企业信息
            List<UserEnterpriseDTO> list = new ArrayList<>();
            enterprisesUsersList.forEach(eu -> {
                Integer userId = managerListDTO.getId();
                // 根据用户id 获取 该用户关联的所有企业信息
                if (userId.equals(eu.getUserId())) {
                    // 每有一个企业就新建一个用户企业关联对象
                    UserEnterpriseDTO userEnterpriseDTO = new UserEnterpriseDTO()
                            .setEnterprise(enterpriseMap.get(eu.getEnterpriseId()))
                            .setIsActive(eu.getIsActive());
                    // 判断该用户是否是该企业管理员
                    if (!ObjectUtils.isEmpty(eu.getRoleId())
                            && (EnumRole.RoleType.superAdmin.getKey().equals(eu.getRoleId())
                            || EnumRole.RoleType.admin.getKey().equals(eu.getRoleId()))) {
                        userEnterpriseDTO.setIsManager(EnumIsManager.IsManager.yes.getKey());
                    } else {
                        userEnterpriseDTO.setIsManager(EnumIsManager.IsManager.not.getKey());
                    }
                    list.add(userEnterpriseDTO);
                }
            });
            managerListDTO.setLinkEnterprise(list);
        });
    }

    @Override
    public ResponseResult<UserListDetailDTO> getUserListDetail(Integer userId, ManagerInfo managerInfo) {
        User user = userMapper.selectOne(new User().setId(userId));
        if (ObjectUtils.isEmpty(user)) {
            log.error("没有该用户或该用户已被禁用，类是：{}，方法是：{}", "UsersServiceImpl", "getUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        UserListDetailDTO userListDetailDTO = BeanUtils.copyProperties(user, UserListDetailDTO.class);
        // 根据用户id查询 关联企业
        List<EnterprisesUsers> enterprisesUsers = enterprisesUsersService.selectList(new EntityWrapper<>(new EnterprisesUsers()
                .setUserId(userId)));

        // 用户关联企业list
        List<UserListDetailEnterpriseDTO> list = new ArrayList<>();
        if (!CollectionUtils.isEmpty(enterprisesUsers)) {
            for (EnterprisesUsers eu : enterprisesUsers) {
                // 根据企业Id查询 企业
                Enterprises enterprise = enterprisesService.selectById(eu.getEnterpriseId());
                if (ObjectUtils.isEmpty(enterprise)) {
                    continue;
                }
                // 用户管理企业相关信息
                UserListDetailEnterpriseDTO detailEnterpriseDTO = new UserListDetailEnterpriseDTO();
                detailEnterpriseDTO
                        // 是否启用
                        .setIsActive(eu.getIsActive())
                        // 企业id 和 企业名字
                        .setId(enterprise.getId())
                        .setName(enterprise.getName());
                // 判断是否是企业管理员
                if (!ObjectUtils.isEmpty(eu.getRoleId())
                        && (EnumRole.RoleType.superAdmin.getKey().equals(eu.getRoleId())
                        || EnumRole.RoleType.admin.getKey().equals(eu.getRoleId()))) {
                    detailEnterpriseDTO.setIsManager(EnumIsManager.IsManager.yes.getKey());
                } else {
                    detailEnterpriseDTO.setIsManager(EnumIsManager.IsManager.not.getKey());
                }
                // 根据userId查询 部门和职位id
                List<UsersDepartments> usersDepartments = usersDepartmentsMapper.selectList(new EntityWrapper<>(new UsersDepartments().setUserId(userId)));
                // 每个部门新建一个角色部门关联对象
                for (UsersDepartments usersDepartment : usersDepartments) {
                    // 查询部门表判断该部门是否当前企业
                    Departments department = departmentsService.selectOne(new EntityWrapper<>(new Departments().setId(usersDepartment.getDepartmentId())));
                    if (ObjectUtils.isEmpty(department)) {
                        continue;
                    }
                    if (department != null && !department.getOrgId().equals(enterprise.getId())) {
                        // 如果当前部门属于当前企业，添加部门和职位信息
                        // 根据id查询职位
                        ZPost post = postService.selectOne(new EntityWrapper<>(new ZPost().setId(usersDepartment.getPostId()).setStatus(EnumCommon.IsActive.is_active.getKey())));
                        if (!ObjectUtils.isEmpty(post)) {
                            detailEnterpriseDTO
                                    // 部门id
                                    .setDepartment(department.getName())
                                    // 职位id
                                    .setPost(post.getName());
                        }
                        break;
                    }
                }
                list.add(detailEnterpriseDTO);
            }
        }
        // 查询省级id 和 市级id
        if (!StringUtils.isEmpty(userListDetailDTO.getRegionid())) {
            ResponseResult<Map<String, Object>> result = commonRegionService.findAllRegionById(Integer.parseInt(userListDetailDTO.getRegionid()));
            // 如果查询成功
            if (result.getCode().equals(1)) {
                Map<String, Object> map = result.getObject();
                userListDetailDTO.setProvinceId(((CommonRegion) map.get("province")).getId().toString())
                        .setCityId(((CommonRegion) map.get("city")).getId().toString());
            }
        }
        userListDetailDTO.setLinkEnterprises(list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userListDetailDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateUserListDetail(UserListDetailRQ rq, ManagerInfo managerInfo) {
        User user = userMapper.selectOne(new User().setId(rq.getId()));
        String nickname = user.getNickname();
        if (ObjectUtils.isEmpty(user)) {
            log.error("没有该用户或该用户已被禁用，类是：{}，方法是：{}", "UsersServiceImpl", "updateUserListDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        // 查询所有用户  姓名、邮箱 是否冲突
        List<User> userActiveList = userMapper.selectList(new EntityWrapper<User>().notIn("id", rq.getId()));
        for (User userActive : userActiveList) {
            if (userActive.getNickname() != null && userActive.getNickname().equals(rq.getNickname())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NAME_ALREADY_EXIST);
            } else if (userActive.getEmail() != null && userActive.getEmail().equals(rq.getEmail())) {
                return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_ALREADY_EXIST);
            }
        }
        // 修改用户基本信息
        org.springframework.beans.BeanUtils.copyProperties(rq, user);
        user.setUpdateAt(new Date());
        if (userMapper.updateById(user) != 1) {
            log.error("更新用户信息失败，类是：{}，方法是：{}", "UsersServiceImpl", "updateUserListDetail");
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.USER_UPDATE_FAILED);
        }
        //发送消息
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("background_admin_username", managerInfo.getNickname());
        sendMessageForPrompt(user.getUsername(), NoticeTemplateType.BACKGROUND_ADMIN_UPDATE_USER_INFO.getKey(), jsonObject);
        //保存消息
        middleSystemNoticeService.createNotice(user.getId(),
                EnumMiddleNoticeTitle.title.USER_CHANGE_DATA_NOTICE.getValue(),
                MiddleNoticeContentTemplate.BACKGROUND_ADMIN_UPDATE_USER_INFO.getKey(),
                new Object[]{managerInfo.getNickname()});

        /**---------记录系统消息-----------*/
        // 拼装系统通知对象
        SystemNotice systemNotice = systemNoticeService.createNotice(managerInfo.getManagerId(), NoticeTemplateType.MANAGER_UPDATE_USER, nickname);
        // 插入系统通知
        if (!systemNoticeService.insert(systemNotice)) {
            log.error("更新企业用户关联信息失败，类是：{}，方法是：{} 913", "UsersServiceImpl", "updateUserListDetail");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
        }

        //是否激活账号
        boolean activeFalg = false;
        // 修改用户企业关联信息
        List<UserListDetailEnterpriseRQ> linkEnterprises = rq.getLinkEnterprises();
        for (UserListDetailEnterpriseRQ enterpriseInfo : linkEnterprises) {
            /**-------------------企业用户关联激活信息-----------------------*/
            // 根据企业id和用户id查询 企业用户关联信息
            List<EnterprisesUsers> enterprisesUsers = enterprisesUsersService.selectList(new EntityWrapper<>(new EnterprisesUsers()
                    .setUserId(rq.getId())
                    .setEnterpriseId(enterpriseInfo.getId())));
            for (EnterprisesUsers eu : enterprisesUsers) {
                // 如果修改了 企业关联用户的 激活状态 则更新
                if (!eu.getIsActive().equals(enterpriseInfo.getIsActive())) {
                    activeFalg = true;
                    // 如果用户在该企业下 是管理员 不能禁用
                    if (eu.getIsActive().equals(EnumCommon.IsActive.is_active.getKey())
                            && enterpriseInfo.getIsManager().equals(EnumIsManager.IsManager.yes.getKey())) {
                        return ResponseResult.buildResponseResult(ResCodeEnum.MANAGER_UNABLE_FORBIDDEN);
                    }
                    eu.setIsActive(enterpriseInfo.getIsActive());
                    // 更新enterprisesUsers
                    if (!enterprisesUsersService.updateById(eu)) {
                        log.error("更新企业用户关联信息失败，类是：{}，方法是：{}", "UsersServiceImpl", "updateUserListDetail");
                        throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.USER_ENTERPRISE_RELATION_UPDATE_FAILED);
                    }

                    /**---------记录系统消息-----------*/
                    // 日志对象
                    OperatorLog operatorLog = new OperatorLog();
                    // 查询企业名称
                    Enterprises enterprise = enterprisesService.selectById(enterpriseInfo.getId());
                    if (enterpriseInfo.getIsActive().equals(EnumCommon.IsActive.is_active.getKey())) {
                        systemNotice = systemNoticeService.createNotice(managerInfo.getManagerId(), NoticeTemplateType.MANAGER_PROHIBIT, enterprise.getName(), nickname, "启用");
                        operatorLog.setOperatorContent(enterprise.getName() + " " + nickname + "已被管理员启用");
                        log.info("{}的{}已被管理员禁用", enterprise.getName(), nickname);
                    } else {
                        systemNotice = systemNoticeService.createNotice(managerInfo.getManagerId(), NoticeTemplateType.MANAGER_PROHIBIT, enterprise.getName(), nickname, "禁用");
                        operatorLog.setOperatorContent(enterprise.getName() + " " + nickname + "已被管理员禁用");
                        log.info("{}的{}已被管理员禁用", enterprise.getName(), nickname);
                    }
                    // 插入系统通知
                    if (!systemNoticeService.insert(systemNotice)) {
                        log.error("更新企业用户关联信息失败，类是：{}，方法是：{} 959", "UsersServiceImpl", "updateUserListDetail");
                        throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.INSERT_SYSTEM_NOTICE_FAILED);
                    }
                    // 后台日志信息
                    operatorLog = new OperatorLog().setOperatorId(managerInfo.getManagerId())
                            .setOperatorRoleId(managerInfo.getRoleInfo().getRoleId())
                            .setOperatorRoleName(managerInfo.getRoleInfo().getRoleName())
                            .setOperatorTime(new Date());
                    operatorLogService.insert(operatorLog);

                }
                //若操作了账号状态，则发送短信通知用户
                if (activeFalg) {
                    Integer noticeContentTemplateKey;
                    if (enterpriseInfo.getIsActive().equals(EnumCommon.IsActive.is_active.getKey())) {
                        sendMessageForPrompt(user.getUsername(), NoticeTemplateType.BACKGROUND_ADMIN_ENABLE_ACCOUNT.getKey(),
                                jsonObject);
                        noticeContentTemplateKey = MiddleNoticeContentTemplate
                                .BACK_ADMINISTRATOR_ENABLE_ACCOUNT.getKey();
                    } else {
                        sendMessageForPrompt(user.getUsername(), NoticeTemplateType.BACKGROUND_ADMIN_DISABLE_ACCOUNT.getKey(),
                                jsonObject);
                        noticeContentTemplateKey = MiddleNoticeContentTemplate
                                .BACK_ADMINISTRATOR_DISABLE_ACCOUNT.getKey();
                    }
                    //保存消息
                    middleSystemNoticeService.createNotice(user.getId(),
                            EnumMiddleNoticeTitle.title.USER_STATUS_CHANGE.getValue(),
                            noticeContentTemplateKey,
                            new Object[]{managerInfo.getNickname()});
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    private User resetUserPassword(String password, String phone) {
        //缓存校验结果
        boolean result = smsService.getCheckResult(phone);
        if (!result) {
            throw new BusinessException(ResCodeEnum.NOT_VALIDATE, ExceptionMessageEnum.NOT_VALIDATE);
        }
        //用户是否存在
        User user = userMapper.selectOne(new User().setUsername(phone));
        //数据有误
        if (user == null || user.getId() == null) {
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NOT_EXIST);
        }
        //是否与旧密码相同
        if (BCryptPassword.matches(password, user.getPassword())) {
            throw new BusinessException(ResCodeEnum.OLD_PASSWORD_SAME_NEW, ExceptionMessageEnum.OLD_PASSWORD_SAME_NEW);
        }
        User newUser = new User().setId(user.getId()).setPassword(BCryptPassword.encode(password)).setUpdateAt(new Date());
        //修改新密码
        userMapper.updateById(newUser);
        jedisService.delKey(ConstantsUtil.VALIDATE_RESULT);
        return user;
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
}

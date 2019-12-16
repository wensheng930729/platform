package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.AuthUserRoleInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.BusinessIdType;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.enums.UserType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtil;
import com.bee.platform.user.authority.dao.mapper.*;
import com.bee.platform.user.authority.entity.*;
import com.bee.platform.user.authority.enums.AuthRoleType;
import com.bee.platform.user.authority.service.*;
import com.bee.platform.user.dao.mapper.UserTokensMapper;
import com.bee.platform.user.entity.UserToken;
import com.bee.platform.user.utils.CenerateIdUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @notes 用户登录凭证
 * @Author junyang.li
 * @Date 17:12 2019/3/4
 **/
@Slf4j
@Service
public class AuthUserTokensServiceImpl extends ServiceImpl<UserTokensMapper, UserToken> implements AuthUserTokensService {

    @Autowired
    private ConfigService configService;

    @Autowired
    private UserTokensMapper userTokensMapper;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private RegionService regionService;

    @Autowired
    private CenerateIdUtils cenerateIdUtils;

    @Autowired
    private AuthPlatformUserService usersService;

    @Autowired
    private AuthEnterpriseService authEnterpriseService;

    @Autowired
    private AuthRoleMapper authRoleMapper;

    @Autowired
    private AuthUserRoleMapper authUserRoleMapper;

    @Autowired
    private AuthEnterpriseRoleMapper authEnterpriseRoleMapper;

    @Autowired
    private AuthFunctionRoleMapper authFunctionRoleMapper;

    @Autowired
    private AuthPlatformUserEnterpriseMapper authPlatformUserEnterpriseMapper;

    @Autowired
    private AuthUsergroupService usergroupService;

    @Autowired
    private AuthUsergroupRoleService usergroupRoleService;

    /**
     * @notes 密码校验成功，获取登录凭证
     * @Author junyang.li
     * @Date 17:13 2019/3/4
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<AuthPlatformUserInfo> getSysToken(AuthPlatformUser user) {
        //地区信息
        RegionDTO dto=regionService.selectRegion(user.getRegionId());
        //查询配置表中token的失效时间的秒数
        AuthPlatformUserInfo userInfo = BeanUtils.copyProperties(user, AuthPlatformUserInfo.class).setRegion(dto);
        //若当前用户为中台或中后台用户
        if (!UserType.BACKGROUND_USER.getKey().equals(user.getUserType())) {
            //查询当前用户已关联的企业
            List<Integer> userEnterpriseIds = authPlatformUserEnterpriseMapper.findUserEnterpriseIds(user.getId());
            if (CollectionUtils.isEmpty(userEnterpriseIds)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_JOIN_ENTERPRISE);
            }
            //若当前用户没有选择企业，则默认选择最新关联的企业
            if (Objects.isNull(user.getCurrentEnterpriseId()) || user.getCurrentEnterpriseId().equals(Status.FALSE.getKey())) {
                user.setCurrentEnterpriseId(userEnterpriseIds.get(0));
            } else if (!userEnterpriseIds.contains(user.getCurrentEnterpriseId())) {
                //若默认企业已不存在，则重新取最新关联的企业
                user.setCurrentEnterpriseId(userEnterpriseIds.get(0));
            }

            AuthEnterprise authEnterprise = authEnterpriseService.selectOne(new EntityWrapper<>(new AuthEnterprise()
                    .setId(user.getCurrentEnterpriseId()).setStatus(Status.TRUE.getKey()).setDeleted(Status.FALSE.getKey())));
            if (Objects.nonNull(authEnterprise)) {
                userInfo.setOrg_name(authEnterprise.getName());
            }

            //若是中后台用户，则查询出用户所在当前企业的角色列表
            if (UserType.MIDDLE_BACKGROUND_USER.getKey().equals(user.getUserType())) {
                getUserRoles(userInfo, authEnterprise.getId());
            }

        }
        userInfo.setOrgId(user.getCurrentEnterpriseId());
        AuthPlatformUser currentUser=new AuthPlatformUser().setId(user.getId());
        int expireSeconds = this.getExpireSeconds();
        //token为空
        boolean tokenIsNull=StringUtils.isEmpty(userInfo.getSysToken());
        //token失效
        boolean tokenIsInvalid=new DateTime(userInfo.getExpiresIn()).isBeforeNow();
        if (tokenIsNull || tokenIsInvalid) {
            // 从新生成并插入
            String newSysToken = this.createSysToken();
            currentUser.setSysToken(newSysToken);
            userInfo.setSysToken(newSysToken);
        }
        currentUser.setExpiresIn(DateUtil.plusSeconds(ConstantsUtil.OVERDUE)).setUpdateTime(new Date())
                .setCurrentEnterpriseId(user.getCurrentEnterpriseId());
        userInfo.setExpiresIn(DateUtil.plusSeconds(expireSeconds)).setExpiresTime(expireSeconds);
        usersService.updateById(currentUser);
        // 放入缓存，并设置失效时间
        jedisService.setObject(userInfo.getSysToken(), userInfo,expireSeconds);
        //统计登录次数
        Integer loginCount = jedisService.incr(ConstantsUtil.LOGIN_COUNT_KEY + userInfo.getSysToken());
        jedisService.setJsonObject(ConstantsUtil.LOGIN_COUNT_KEY + userInfo.getSysToken(), loginCount, expireSeconds);
        log.info("username={},放入缓存中的数据userInfo={}", user.getUsername(), userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,userInfo);
    }


    /**
     * @notes 为管理用户生成业务id
     * @Author junyang.li
     * @Date 15:37 2019/4/25
     **/
    private String createBeesrvId(){
        try {
            return cenerateIdUtils.generateOrderId(BusinessIdType.USER);
        }catch (Exception e){
            log.error("调用缓存生成管理用户的业务id连接异常，异常信息是:{}",e);
            int count=usersService.selectCount(new EntityWrapper<>());
            return cenerateIdUtils.generateOrderId(BusinessIdType.USER,count);
        }
    }

    /**
     * @notes 通过username修改UserToken中的字段
     * @Author junyang.li
     * @Date 10:02 2019/3/5
     **/
    @Override
    public void updateByParam(UserToken userToken) {
        if(userToken==null||StringUtils.isEmpty(userToken.getUsername())){
            return ;
        }
        userTokensMapper.updateByParam(userToken.setUpdateTime(new Date()));
    }

    /**
     * @notes 生成新的sysToken
     * @Author junyang.li
     * @Date 17:28 2019/3/4
     **/
    @Override
    public  String createSysToken(){
        String uuid= UUID.randomUUID().toString();
        String code=RandomStringUtils.randomAlphanumeric(6);
        return new StringBuilder(ConstantsUtil.PLATFORM).append(uuid).append(ConstantsUtil.LINE)
                .append(code).toString();
    }

    /**
     * @notes 获取sysToken过期时间秒数
     * @Author junyang.li
     * @Date 15:40 2019/3/19
     **/
    @Override
    public int getExpireSeconds() {
        String val=configService.getConfValue(ConstantsUtil.TOKEN_EXPIRES_IN, "7200", "Token 在redis在的失效时间");
        return StringUtils.isEmpty(val)?0:Integer.valueOf(val);
    }

    /**
     * 获取用户所在用户组中的角色列表
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
            authUserRoles.forEach(userRole -> {roleIds.add(userRole.getRoleId());});
        }
        //查询用户所在用户组中的角色
        List<AuthUsergroupRole> userGroupRoles = getUserGroupRoles(userInfo.getId(), orgId);
        if (CollectionUtils.isNotEmpty(userGroupRoles)) {
            userGroupRoles.forEach(groupRole -> {roleIds.add(groupRole.getRoleId());});
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
}

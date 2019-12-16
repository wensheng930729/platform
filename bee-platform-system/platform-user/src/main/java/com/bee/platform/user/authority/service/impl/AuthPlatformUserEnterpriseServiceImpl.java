package com.bee.platform.user.authority.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.dto.RegionDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.enums.FreeMarkerType;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.enums.UserActiveType;
import com.bee.platform.common.enums.UserType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.user.authority.dao.mapper.AuthPlatformUserEnterpriseMapper;
import com.bee.platform.user.authority.dao.mapper.AuthPlatformUserMapper;
import com.bee.platform.user.authority.dao.mapper.AuthUserRoleMapper;
import com.bee.platform.user.authority.dto.AuthEnterpriseDepartmentPostDto;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.authority.dto.AuthPlatformUserDto;
import com.bee.platform.user.authority.dto.AuthPlatformUserEnterpriseDTO;
import com.bee.platform.user.authority.dto.AuthUserEnterpriseDetailDto;
import com.bee.platform.user.authority.dto.AuthUserEnterprisesDto;
import com.bee.platform.user.authority.entity.AuthCommonFile;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
import com.bee.platform.user.authority.entity.AuthRole;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.authority.enums.AuthRoleType;
import com.bee.platform.user.authority.enums.EnumEnterpriseUserType;
import com.bee.platform.user.authority.enums.EnumFileType;
import com.bee.platform.user.authority.enums.EnumNewCommon;
import com.bee.platform.user.authority.rq.AuthPlatformUserDepartmentAndPostRQ;
import com.bee.platform.user.authority.rq.AuthPlatformUserRQ;
import com.bee.platform.user.authority.rq.AuthPlatformUserSelectRQ;
import com.bee.platform.user.authority.service.AuthCommonFileService;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthRoleService;
import com.bee.platform.user.authority.service.AuthUserRoleService;
import com.bee.platform.user.authority.service.AuthUserTokensService;
import com.bee.platform.user.dao.mapper.DepartmentsMapper;
import com.bee.platform.user.email.MailService;
import com.bee.platform.user.entity.Departments;
import com.bee.platform.user.service.DepartmentsService;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.DepartmentUtil;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 企业与用户中间表 服务实现类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthPlatformUserEnterpriseServiceImpl extends ServiceImpl<AuthPlatformUserEnterpriseMapper, AuthPlatformUserEnterprise> implements AuthPlatformUserEnterpriseService {

    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private AuthPlatformUserEnterpriseMapper userEnterpriseMapper;
    @Autowired
    private AuthPlatformUserMapper authPlatformUserMapper;
    @Autowired
    private AuthPlatformUserEnterpriseMapper authPlatformUserEnterpriseMapper;
    @Autowired
    private MailService mailService;
    @Autowired
    private SmsService smsService;
    @Autowired
    private AuthUserRoleService authUserRoleService;
    @Autowired
    private RegionService regionService;
    @Autowired
    private AuthEnterpriseService enterpriseService;
    @Autowired
    private AuthCommonFileService commonFileService;
    @Autowired
    private AuthRoleService authRoleService;
    @Autowired
    private AuthPlatformUserService authPlatformUserService;
    
    @Autowired
    private AuthUserRoleMapper authUserRoleMapper;

    @Autowired
    private JedisService jedisService;

    @Autowired
    private AuthUserTokensService authUserTokensService;

    @Autowired
    private DepartmentsService departmentsService;
    
    @Autowired
    private DepartmentsMapper departmentsMapper;

    @Override
    public List<AuthPlatformUser> getEnterpriseUser(Integer enterpriseId) {
        List<AuthPlatformUserEnterprise> userEnterprises = userEnterpriseMapper.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setEnterpriseId(enterpriseId)
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumNewCommon.IsActive.is_active.getKey())));
        if (CollectionUtils.isEmpty(userEnterprises)) {
            return Lists.newArrayList();
        }
        List<Integer> userIds = userEnterprises.stream().map(AuthPlatformUserEnterprise::getUserId).collect(Collectors.toList());
        return userService.selectList(new EntityWrapper<>(new AuthPlatformUser()
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey()))
                .in("id", userIds));
    }

    /**
     * 查询企业管理员
     */
    @Override
    public List<AuthPlatformUser> getEnterpriseAdmin(Integer enterpriseId) {
        List<AuthPlatformUserEnterprise> userEnterpriseList = selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setEnterpriseId(enterpriseId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<Integer> ids = userEnterpriseList.stream().map(AuthPlatformUserEnterprise::getUserId).collect(Collectors.toList());
        List<AuthUserRole> userRoles = authUserRoleService.selectList(new EntityWrapper<>(new AuthUserRole()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setRoleType(EnumRoleType.ENTERPRISE_ADMIN.getCode()))
                .in("user_id", ids));
        List<Integer> userIds = userRoles.stream().map(AuthUserRole::getUserId).collect(Collectors.toList());
        return userService.selectList(new EntityWrapper<>(new AuthPlatformUser()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey()))
                .in("id", userIds));
    }

    /**
     * 查询企业管理员的ids
     */
    @Override
    public List<Integer> getEnterpriseAdminIds(Integer enterpriseId) {
        List<AuthPlatformUserEnterprise> userEnterpriseList = selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setEnterpriseId(enterpriseId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<Integer> ids = userEnterpriseList.stream().map(AuthPlatformUserEnterprise::getUserId).collect(Collectors.toList());
        List<AuthUserRole> userRoles = authUserRoleService.selectList(new EntityWrapper<>(new AuthUserRole()
                .setEnterpriseId(enterpriseId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setRoleType(EnumRoleType.ENTERPRISE_ADMIN.getCode()))
                .in("user_id", ids));
        return userRoles.stream().map(AuthUserRole::getUserId).collect(Collectors.toList());
    }

    @Override
    public List<AuthPlatformUser> getEnterpriseCommonUser(Integer enterpriseId) {
        List<AuthPlatformUserEnterprise> userEnterpriseList = selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setEnterpriseId(enterpriseId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<Integer> ids = userEnterpriseList.stream().map(a -> a.getUserId()).collect(Collectors.toList());
        List<AuthUserRole> userRoles = authUserRoleService.selectList(new EntityWrapper<>(new AuthUserRole()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setRoleType(EnumRoleType.ENTERPRISE_ADMIN.getCode()))
                .in("user_id", ids));
        List<Integer> userIds = userRoles.stream().map(a -> a.getUserId()).collect(Collectors.toList());
        return userService.selectList(new EntityWrapper<>(new AuthPlatformUser()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey()))
                .in("id", ids).notIn("id", userIds));
    }

    /**
     * 根据企业id和用户id查询
     */
    @Override
    public List<AuthPlatformUserEnterpriseDTO> qureyEnterpriseUser(int userId) {
        ArrayList<AuthPlatformUserEnterpriseDTO> arrayList = new ArrayList<>();
        AuthPlatformUserEnterpriseDTO authPlatformUserEnterpriseDTO = new AuthPlatformUserEnterpriseDTO();
        List<AuthPlatformUserEnterprise> list = authPlatformUserEnterpriseMapper.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setUserId(userId)));
        for (AuthPlatformUserEnterprise authPlatformUserEnterprise : list) {
            BeanUtils.copyProperties(authPlatformUserEnterprise, authPlatformUserEnterpriseDTO);
            arrayList.add(authPlatformUserEnterpriseDTO);
        }
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }
        return arrayList;

    }

    @Override
    public ResponseResult<List<AuthUserEnterprisesDto>> getUserEnterprises(AuthPlatformUserInfo userInfo) {
        // 查询用户企业的关联关系
        List<AuthPlatformUserEnterprise> userEnterprises = userEnterpriseMapper.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setUserId(userInfo.getId())
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumNewCommon.IsActive.is_active.getKey())));
        if (CollectionUtils.isEmpty(userEnterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, Lists.newArrayList());
        }
        // 当前登录人所在企业的 企业ids
        List<Integer> enterpriseIds = userEnterprises.stream().map(a -> a.getEnterpriseId()).collect(Collectors.toList());
        List<AuthEnterprise> enterprises = enterpriseService.selectList(new EntityWrapper<>(new AuthEnterprise()
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey()))
                .in("id", enterpriseIds).orderBy("pid,id", true));
        List<AuthUserEnterprisesDto> list = Lists.newArrayList();
        int size = enterprises.size();
        // 遍历设置logo和address
        for (int i = 0; i < size; i++) {
            AuthEnterprise enterprise = enterprises.get(i);
            AuthUserEnterprisesDto dto = BeanUtils.copyProperties(enterprise, AuthUserEnterprisesDto.class);
            AuthCommonFile logo = commonFileService.selectOne(new EntityWrapper<>(new AuthCommonFile()
                    .setEnterprisesId(enterprise.getId()).setType(EnumFileType.FileType.logo.getKey())));
            if (!ObjectUtils.isEmpty(logo)) {
                dto.setHead(logo.getUrl());
            }
            dto.setAddress(enterpriseService.getRegion(enterprise, Maps.newHashMap()))
                    .setUserNum(userInfo.getPhone());
            list.add(dto);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    /**
     * @Description 条件查询平台用户列表
     * @Param authPlatformUserSelectRQ
     * @Param page
     * @Date 2019/5/21 9:43
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<AuthPlatformUserDto> getList(String sysToken,
                                             AuthPlatformUserSelectRQ authPlatformUserSelectRQ,
                                             Pagination pagination) {
        //获取登录人信息
        AuthPlatformUserInfo selfInfo = userService.getSelfInfo(sysToken);

        List<AuthEnterpriseFlatDTO> enterprises = enterpriseService.getEnterpriseFlatByUser(selfInfo).getObject();
        if (CollectionUtils.isNotEmpty(enterprises)) {
            List<Integer> enterpriseIds = new ArrayList<>();
            for (AuthEnterpriseFlatDTO enterprise : enterprises) {
                enterpriseIds.add(enterprise.getValue());
            }
            authPlatformUserSelectRQ.setEnterpriseIds(enterpriseIds);
        }
        List<AuthPlatformUserDto> userList = authPlatformUserMapper.findList(authPlatformUserSelectRQ, pagination);
        if (CollectionUtils.isNotEmpty(userList)) {
            userList.forEach(user -> {
                if (Objects.nonNull(user.getDepartmentId())) {
                    StringBuffer departments = new StringBuffer();
                    List<Departments> parentDaparment = departmentsService.getParentDaparment(user.getDepartmentId());
                    if (CollectionUtils.isNotEmpty(parentDaparment)) {
                        parentDaparment.forEach(dp -> {departments.append(dp.getName()).append("/");});
                    }
                    departments.append(user.getDepartmentName());
                    user.setDepartmentName(departments.toString());
                }
            });
        }
        return userList;
    }

    /**
     * 根据企业id 查询企业下所有的人员
     */
    @Override
    public List<AuthPlatformUserDto> getListOfEnterprise(AuthPlatformUserSelectRQ rq, Pagination pagination){
        List<AuthPlatformUserDto> userDtos = authPlatformUserMapper.listByenterpriseId(rq,pagination);

        List<AuthUserRole> authRoles = authUserRoleService.selectList(new EntityWrapper<>(new AuthUserRole()
                .setDeleted(Status.FALSE.getKey()).setEnterpriseId(rq.getEnterpriseId())
                .setRoleType("enterprise_admin")));
        for (AuthPlatformUserDto u : userDtos){
            for (AuthUserRole userRole : authRoles){
                if (userRole.getUserId().equals(u.getId())){
                    u.setAdmin(Boolean.TRUE.booleanValue());
                    break;
                }
            }
        }
        return userDtos;
    }

    /**
     * @Description 添加企业用户
     * @Param authPlatformUserRQ
     * @Date 2019/5/21 10:00
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> add(String sysToken, AuthPlatformUserRQ authPlatformUserRQ) {
        //获取登录人信息
        AuthPlatformUserInfo selfInfo = userService.getSelfInfo(sysToken);
        if (Objects.isNull(selfInfo.getManagerType())
                || !AuthRoleType.ENTERPRISE_ADMIN.getCode().equals(selfInfo.getManagerType())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_ADMIN);
        }
        //查询该用户是否存在
        AuthPlatformUser existUser = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setUsername(authPlatformUserRQ.getUsername())
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey()));
        if (Objects.nonNull(existUser)) {
            //查询该用户是否已关联企业
            List<AuthPlatformUserEnterprise> authPlatformUserEnterpriseList = authPlatformUserEnterpriseMapper
                    .selectList(new EntityWrapper<AuthPlatformUserEnterprise>()
                            .eq("user_id", existUser.getId())
                            .and()
                            .eq("status", Status.TRUE.getKey())
                            .and()
                            .eq("deleted", Status.FALSE.getKey()));
            if (CollectionUtils.isNotEmpty(authPlatformUserEnterpriseList)) {
                //获取当前登录用户所在企业及子企业
                List<AuthEnterpriseFlatDTO> enterprises = enterpriseService.getEnterpriseFlatByUser(selfInfo).getObject();
                if (CollectionUtils.isNotEmpty(enterprises)) {
                    List<Integer> enterpriseIds = new ArrayList<>();
                    enterprises.forEach(enterprise -> {
                        enterpriseIds.add(enterprise.getValue());
                    });

                    List<AuthPlatformUserEnterprise> enterpriseList = authPlatformUserEnterpriseList.stream().filter(ep -> {
                        return enterpriseIds.contains(ep.getEnterpriseId());
                    }).collect(Collectors.toList());
                    //判断若当前登录用户在子企业，添加的用户已关联母企业，则不提示，否则提示已关联企业
                    if (CollectionUtils.isNotEmpty(enterpriseList)) {
                        return ResponseResult.buildResponseResult(ResCodeEnum.USER_AT_ALREADY_ENTERPRISE);
                    }
                }
                //查询当前登录人所在集团
//                AuthEnterprise currentParent = enterpriseService.getAncestor(selfInfo.getOrgId());
//                //查询已关联的集团
//                AuthEnterprise oldParent = enterpriseService.getAncestor(authPlatformUserEnterpriseList.get(0).getEnterpriseId());
//                if (!currentParent.getId().equals(oldParent.getId())) {
//                    return ResponseResult.buildResponseResult(ResCodeEnum.USER_AT_OTHER);
//                }
            }
            Integer userType = UserType.MIDDLE_USER.getKey();
            //判断该用户是否是后台用户，若是，则设置该用户为中后台用户类型
            if (Objects.nonNull(existUser.getUserType()) && UserType.BACKGROUND_USER.getKey().equals(existUser.getUserType())) {
                userType = UserType.MIDDLE_BACKGROUND_USER.getKey();
            }
            AuthPlatformUser authPlatformUser = BeanUtils.copyProperties(authPlatformUserRQ, AuthPlatformUser.class)
                    .setId(existUser.getId())
                    .setUserType(userType)
                    .setStatus(Status.TRUE.getKey())
                    .setUpdateTime(new Date())
                    .setNickname(authPlatformUserRQ.getName());
            authPlatformUserMapper.updateById(authPlatformUser);
            //添加用户关联的企业部门职位
            if (CollectionUtils.isNotEmpty(authPlatformUserRQ.getDepartmentAndPostList())) {
                batchInsert(existUser.getId(), authPlatformUserRQ, selfInfo.getId());
            }
        } else {
            //如果该用户不存在
            AuthPlatformUser authPlatformUser = BeanUtils.copyProperties(authPlatformUserRQ, AuthPlatformUser.class)
                    .setCreateTime(new Date()).setStatus(Status.TRUE.getKey())
                    .setPhone(authPlatformUserRQ.getUsername())
                    .setUserType(UserType.MIDDLE_USER.getKey())
                    .setActiveType(UserActiveType.PLATFORM_ADD.getKey())
                    .setPassword(BCryptPassword.encode("123456"))
                    .setNickname(authPlatformUserRQ.getName())
                    .setUpdateTime(new Date());
            authPlatformUserMapper.insert(authPlatformUser);
            //添加用户关联的企业部门职位
            if (CollectionUtils.isNotEmpty(authPlatformUserRQ.getDepartmentAndPostList())) {
                batchInsert(authPlatformUser.getId(), authPlatformUserRQ, selfInfo.getId());
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 修改企业用户
     * @Param id
     * @Param authPlatformUserRQ
     * @Date 2019/5/21 10:03
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> update(String sysToken, Integer id, AuthPlatformUserRQ authPlatformUserRQ) {
        //获取登录人信息
        AuthPlatformUserInfo selfInfo = userService.getSelfInfo(sysToken);
        if ((Objects.isNull(selfInfo.getManagerType())
                || !AuthRoleType.ENTERPRISE_ADMIN.getCode().equals(selfInfo.getManagerType()))
                && !selfInfo.getId().equals(id)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_ADMIN);
        }
        //查询当前登录用户所在企业及子企业
        List<AuthEnterpriseFlatDTO> enterpriseList = enterpriseService.getEnterpriseFlatByCompanyId(authPlatformUserRQ.getCompanyId()).getObject();
        List<Integer> companyIds = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(enterpriseList)) {
            for (AuthEnterpriseFlatDTO enterprise : enterpriseList) {
                companyIds.add(enterprise.getValue());
            }
        }

        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectById(id);
        if (Objects.isNull(authPlatformUser)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PLATFORM_USER_NOT_EXIST);
        }
        AuthPlatformUser newAuthPlatformUser = BeanUtils.copyProperties(authPlatformUserRQ,
                AuthPlatformUser.class).setId(id).setUpdateTime(new Date());
        authPlatformUserMapper.updateById(newAuthPlatformUser);
        //若当前修改的用户是当前登录人，则将最新信息更新到缓存
        if (newAuthPlatformUser.getId().equals(selfInfo.getId())) {
            if (Objects.nonNull(authPlatformUserRQ.getRegionId())) {
                //地区信息
                RegionDTO region = regionService.selectRegion(authPlatformUserRQ.getRegionId());
                selfInfo.setRegion(region);
            }
            selfInfo.setName(newAuthPlatformUser.getName())
                    .setEmail(newAuthPlatformUser.getEmail())
                    .setFixtel(newAuthPlatformUser.getFixtel())
                    .setRegionId(newAuthPlatformUser.getRegionId())
                    .setAddress(newAuthPlatformUser.getAddress())
                    .setHead(newAuthPlatformUser.getHead());
            jedisService.delKey(sysToken);
            jedisService.setObject(sysToken, selfInfo, authUserTokensService.getExpireSeconds());
        }
        //查询用户已关联的企业
//        List<AuthPlatformUserEnterprise> authPlatformUserEnterprises = authPlatformUserEnterpriseMapper
//                .selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
//                .setUserId(id)
//                .setDeleted(Status.FALSE.getKey())));
        List<AuthPlatformUserEnterprise> authPlatformUserEnterprises = authPlatformUserEnterpriseMapper
                .selectList(new EntityWrapper<AuthPlatformUserEnterprise>().eq("user_id", id).and()
                .in("enterprise_id", companyIds).and()
                .eq("deleted", Status.FALSE.getKey())
                .and().eq("status", Status.TRUE.getKey()));
        if (CollectionUtils.isNotEmpty(authPlatformUserRQ.getDepartmentAndPostList())) {
            //过滤出已删除的关联企业
            if (CollectionUtils.isNotEmpty(authPlatformUserEnterprises)) {
                authPlatformUserEnterprises = authPlatformUserEnterprises.stream().filter(oldEnterprise -> {
                    return authPlatformUserRQ.getDepartmentAndPostList().stream().filter(newEnterprise ->
                            newEnterprise.getEnterpriseId().equals(oldEnterprise.getEnterpriseId())).count() <= 0;
                }).collect(Collectors.toList());
            }
            //删除当前用户所关联的老企业
            authPlatformUserEnterpriseMapper.update(new AuthPlatformUserEnterprise().setDeleted(Status.TRUE.getKey()),
                    new EntityWrapper<AuthPlatformUserEnterprise>()
                    .eq("user_id", id).and().in("enterprise_id", companyIds));
            //添加当前用户所关联的新企业
            batchInsert(id, authPlatformUserRQ, selfInfo.getId());
        }
        //删除已删除的关联的企业下的用户角色
        if (CollectionUtils.isNotEmpty(authPlatformUserEnterprises)) {
            List<Integer> enterpriseIds = new ArrayList<>();
            authPlatformUserEnterprises.forEach(enterprises -> {
                enterpriseIds.add(enterprises.getId());
            });
            authUserRoleService.update(new AuthUserRole().setDeleted(Status.TRUE.getKey()), new EntityWrapper<AuthUserRole>()
                    .eq("user_id", id)
                    .and()
                    .in("enterprise_id", enterpriseIds));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 批量删除企业用户
     * @Param ids
     * @Date 2019/5/21 10:06
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> batchDelete(String ids) {
        String[] idArray = ids.split(",");
        if (idArray.length <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PLATFORM_USER_ID_EMPTY);
        }
        List<Integer> idList = new ArrayList<Integer>();
        for (String id : idArray) {
            idList.add(Integer.valueOf(id));
        }
        if (authPlatformUserMapper.deleteBatchIds(idList) < 0) {
            log.error("删除平台用户失败,调用方法是{}", "AuthPlatformUserServiceImpl.batchDelete()");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 更新企业用户状态
     * @Param id
     * @Date 2019/5/23 10:47
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateStatus(String sysToken, Integer userId,
                                                    Integer enterpriseId, Integer status) {
        //获取登录人信息
        AuthPlatformUserInfo selfInfo = userService.getSelfInfo(sysToken);
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setId(userId).setDeleted(Status.FALSE.getKey()));
        Integer updateFlag = authPlatformUserEnterpriseMapper.update(new AuthPlatformUserEnterprise()
                        .setStatus(status),
                new EntityWrapper<AuthPlatformUserEnterprise>().eq("user_id", userId)
                        .eq("enterprise_id", enterpriseId));
        if (updateFlag < 0) {
            log.error("修改平台用户状态失败,调用方法是{}", "AuthPlatformUserServiceImpl.updateStatus()");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_PLATFORM_USER_STATUS);
        }
        //启用
        if (Status.TRUE.getKey().equals(status)) {
            sendSmsAfterIsActiveUser(authPlatformUser.getUsername(), selfInfo.getNickname(),
                    NoticeTemplateType.ENTERPRISE_ADMIN_ENABLE_ACCOUNT.getKey());
        } else {
            //禁用
            sendSmsAfterIsActiveUser(authPlatformUser.getUsername(), selfInfo.getNickname(),
                    NoticeTemplateType.ENTERPRISE_ADMIN_DISABLE_ACCOUNT.getKey());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> resetMemberPassword(String sysToken, Integer userId,
                                                           Integer type, Integer enterpriseId) {
        //获取当前登录用户信息
        AuthPlatformUserInfo selfInfo = userService.getSelfInfo(sysToken);
        //查询用户是否存在
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser().setId(userId));
        if (Objects.isNull(authPlatformUser)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        AuthPlatformUserEnterprise authPlatformUserEnterprise = authPlatformUserEnterpriseMapper
                .selectOne(new AuthPlatformUserEnterprise()
                        .setUserId(userId).setEnterpriseId(enterpriseId)
                        .setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(authPlatformUserEnterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_USER_NOT_FOUND);
        }
        if (authPlatformUserEnterprise.getStatus().equals(Status.FALSE.getKey())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PLATFORM_USER_DISABLE);
        }
        //随机生成6位密码
        String password = RandomStringUtils.randomAlphanumeric(6);
        //加密
        String encode = BCryptPassword.encode(password);
        authPlatformUserMapper.updateById(new AuthPlatformUser()
                .setId(userId).setPassword(encode).setUpdateTime(new Date()));
        Map<String, Object> map = new HashMap<>(2);
        map.put("password", password);
        map.put("enterprise_admin_username", selfInfo.getNickname());
        //发送短信给用户新的密码
        if (Status.FALSE.getKey().equals(type)) {
            try {
                smsService.sendMessageForPrompt(authPlatformUser.getUsername(),
                        NoticeTemplateType.ENTERPRISE_ADMIN_RESET_PASSWORD.getKey(), map);
            } catch (Exception e) {
                log.error("发生短信时客户端连接失败，异常信息是：{}", e);
                return ResponseResult.buildResponseResult(ResCodeEnum.PASSWORD_RESET_FAIL);
            }
        } else {
            //发送邮件
            String email = authPlatformUser.getEmail();
            if (StringUtils.isBlank(email)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.EMAIL_NOT_RESET_PASSWORD);
            }
            map.put("time", DateUtils.format(new Date(), DateUtils.DEFAULT));
            mailService.sendMail(email, FreeMarkerType.PASSWORD_EMAIL, map);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @param list :
     * @notes: 批量插入用户企业关系数据
     * @Author: junyang.li
     * @Date: 13:54 2019/5/24
     * @return: void
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void insertAll(List<AuthPlatformUserEnterprise> list) {
        if (!CollectionUtils.isEmpty(list)) {
            userEnterpriseMapper.batchInsert(list);
        }
    }

    private void batchInsert(Integer userId, AuthPlatformUserRQ authPlatformUserRQ, Integer createUser) {
        List<AuthPlatformUserDepartmentAndPostRQ> list = authPlatformUserRQ.getDepartmentAndPostList();
        List<AuthPlatformUserEnterprise> userEnterpriseList = new ArrayList<AuthPlatformUserEnterprise>();
        //企业管理员所在企业id
        List<Integer> enterpriseAdminIds = new ArrayList<>();
        //企业成员所在企业id
        List<Integer> enterpriseMemberIds = new ArrayList<>();
        List<AuthUserRole> authUserRoles = null;
        AuthUserRole authUserRole;
        Integer roleId = 0;
        AuthRole authRole = authRoleService.selectOne(new EntityWrapper<AuthRole>()
                .eq("role_type", EnumRoleType.ENTERPRISE_ADMIN.getCode())
                .eq("deleted", Status.FALSE.getKey()));
        if (Objects.nonNull(authRole)) {
            authUserRoles = new ArrayList<>();
            roleId = authRole.getId();
        }
        AuthPlatformUserEnterprise userEnterprise;
        for (AuthPlatformUserDepartmentAndPostRQ dp : list) {
            userEnterprise = new AuthPlatformUserEnterprise();
            userEnterprise.setUserId(userId)
                    .setEnterpriseId(dp.getEnterpriseId())
                    .setDepartmentsId(dp.getDepartmentId())
                    .setPostId(dp.getPostId())
                    .setStatus(Status.TRUE.getKey())
                    .setCreateTime(new Date())
                    .setUpdateTime(new Date())
                    .setDeleted(Status.FALSE.getKey());
            userEnterpriseList.add(userEnterprise);

            if (EnumEnterpriseUserType.ENTERPRISE_ADMIN.getCode().equals(dp.getUserType())) {
                authUserRole = new AuthUserRole();
                authUserRole.setUserId(userId);
                authUserRole.setEnterpriseId(dp.getEnterpriseId());
                authUserRole.setRoleType(EnumRoleType.ENTERPRISE_ADMIN.getCode());
                authUserRole.setCreateUser(createUser);
                authUserRole.setCreateTime(new Date());
                authUserRole.setUpdateTime(new Date());
                authUserRole.setRoleId(roleId);
                authUserRole.setPid(0);
                authUserRoles.add(authUserRole);
                enterpriseAdminIds.add(dp.getEnterpriseId());
            } else {
                enterpriseMemberIds.add(dp.getEnterpriseId());
            }
        }
        authPlatformUserEnterpriseMapper.batchInsert(userEnterpriseList);
        //若是企业成员,则删除原有的可能存在的管理员角色
        if (CollectionUtils.isNotEmpty(enterpriseMemberIds)) {
            authUserRoleService.update(new AuthUserRole().setDeleted(Status.TRUE.getKey()),
                    new EntityWrapper<AuthUserRole>().eq("user_id", userId)
                            .and()
                            .eq("role_type", EnumRoleType.ENTERPRISE_ADMIN.getCode())
                            .in("enterprise_id", enterpriseMemberIds));
        }
        //若是企业管理员
        if (CollectionUtils.isNotEmpty(authUserRoles)) {
            //删除所关联企业下的角色
            authUserRoleService.update(new AuthUserRole().setDeleted(Status.TRUE.getKey()),
                    new EntityWrapper<AuthUserRole>().eq("user_id", userId)
                            .and()
                            .in("enterprise_id", enterpriseAdminIds));
            //添加企业管理员角色
            authUserRoleService.insertBatch(authUserRoles);
        }
    }

    /**
     * @Description 查询该用户的企业id
     * @Param userId
     * @Param orgIds
     * @Date 2019/5/24 14:01
     * @Author xin.huang
     * @Return
     */
    @Override
    public List<Integer> userInEnterprises(Integer userId, List<Integer> orgIds) {
        List<AuthPlatformUserEnterprise> enterprisesUsers = baseMapper.selectList(new EntityWrapper<AuthPlatformUserEnterprise>()
                .where("user_id={0}", userId).and().in("enterprise_id", orgIds));
        if (CollectionUtils.isEmpty(enterprisesUsers)) {
            return Collections.emptyList();
        }
        return enterprisesUsers.stream().map(AuthPlatformUserEnterprise::getEnterpriseId).collect(Collectors.toList());
    }

    /**
     * @Description 根据用户id和企业id查询企业信息
     * @Param userId
     * @Param enterpriseId
     * @Date 2019/5/24 14:53
     * @Author xin.huang
     * @Return
     */
    @Override
    public AuthPlatformUserEnterprise findByUserIdAndEnterpriseId(Integer userId, Integer enterpriseId) {
        Wrapper<AuthPlatformUserEnterprise> wrapper = new EntityWrapper<AuthPlatformUserEnterprise>();
        wrapper.eq("user_id", userId)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey());
        return selectOne(wrapper);
    }

    /**
     * @Description 查询用户所在企业的信息详情
     * @Param userId
     * @Param enterpriseId
     * @Date 2019/5/27 14:03
     * @Author xin.huang
     * @Return
     */
    @Override
    public AuthPlatformUserDto findUserEnterpriseInfo(Integer userId, Integer enterpriseId) {
        return authPlatformUserEnterpriseMapper.findUserEnterpriseInfo(userId, enterpriseId);
    }

    /**
     * @Description 查询用户所在企业的信息和权限详情
     * @Param userId
     * @Param enterpriseId
     * @Date 2019/5/27 16:14
     * @Author xin.huang
     * @Return
     */
    @Override
    public AuthUserEnterpriseDetailDto findUserEnterpriseAndResourceInfo(AuthPlatformUserInfo userInfo, Integer userId) {
        //查询当前用户所在企业及子企业
        List<AuthEnterpriseFlatDTO> enterpriseList = enterpriseService.getEnterpriseFlatByUser(userInfo).getObject();
        List<Integer> enterpriseIds = null;
        if (CollectionUtils.isNotEmpty(enterpriseList)) {
            enterpriseIds = new ArrayList<>();
            for (AuthEnterpriseFlatDTO enterprise : enterpriseList) {
                enterpriseIds.add(enterprise.getValue());
            }
        }
        AuthPlatformUser authPlatformUser = authPlatformUserMapper.selectOne(new AuthPlatformUser()
                .setId(userId).setDeleted(Status.FALSE.getKey()));
        if (Objects.isNull(authPlatformUser)) {
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        AuthUserEnterpriseDetailDto authUserEnterpriseDetailDto = BeanUtils
                .copyProperties(authPlatformUser, AuthUserEnterpriseDetailDto.class);
        //查询区域信息
        if (Objects.nonNull(authPlatformUser.getRegionId())) {
            authUserEnterpriseDetailDto.setRegionInfo(regionService.selectRegion(authPlatformUser.getRegionId()));
        }
        if (CollectionUtils.isEmpty(enterpriseIds)) {
            return authUserEnterpriseDetailDto;
        }
        //查询用户当前所在企业及子企业的部门与职位信息
        List<AuthEnterpriseDepartmentPostDto> userDepartmentAndPostInfos = authPlatformUserEnterpriseMapper
                .findUserDepartmentAndPostInfo(userId, enterpriseIds);
        if (CollectionUtils.isEmpty(userDepartmentAndPostInfos)) {
            return authUserEnterpriseDetailDto;
        }

        userDepartmentAndPostInfos.forEach(dp -> {
            Boolean manager = authPlatformUserService.isManager(dp.getEnterpriseId(),
                    userId, EnumRoleType.ENTERPRISE_ADMIN.getCode());
            if (manager) {
                dp.setUserType(EnumEnterpriseUserType.ENTERPRISE_ADMIN.getCode());
            }
            //查询所在部门的父级信息
            List<Departments> parentDaparment = departmentsService.getParentDaparment(dp.getDepartmentId());
            List<Integer> departmentIds = new ArrayList<>();
            if (CollectionUtils.isNotEmpty(parentDaparment)) {
                parentDaparment.forEach(p -> {
                    departmentIds.add(p.getId());
                });
            }
            departmentIds.add(dp.getDepartmentId());
            dp.setDepartmentIds(departmentIds);
        });
        authUserEnterpriseDetailDto.setEnterpriseDepartmentPostList(userDepartmentAndPostInfos);
        return authUserEnterpriseDetailDto;
    }

    private void sendSmsAfterIsActiveUser(String phone, String enterpriseAdminUsername, Integer key) {
        try {
            if (!Validator.isMobile(phone)) {
                log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
            }
            //组装参数
            JSONObject sendJson = new JSONObject();
            sendJson.put("enterprise_admin_username", enterpriseAdminUsername);
            smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());
        } catch (Exception e) {
            log.info(e.getMessage());
        }
    }
    /**
     * 根据企业和部门id查询 用户ID
     */
    @Override
    public ResponseResult<Set<Integer>> findDepartmentIdAndEnterpriseId(List<Integer> departmentIds, Integer enterpriseId) {
		List<Integer> departmentIdList = new ArrayList<>();
		Set<Integer> userIds = Sets.newHashSet();
		try {
		    
		    // 获取公司下所有部门
		    List<Departments> allDepartments = departmentsMapper.selectList(new EntityWrapper<Departments>().eq("org_id", enterpriseId));
		    if (!org.springframework.util.CollectionUtils.isEmpty(departmentIds)) {
		        departmentIdList.addAll(departmentIds);
		        departmentIds.forEach(d -> DepartmentUtil.getTree(allDepartments, d, departmentIdList));
		    }
		    
		    List<AuthPlatformUserEnterprise> list = authPlatformUserEnterpriseMapper
		        .selectList(new EntityWrapper<AuthPlatformUserEnterprise>().in("departments_id", departmentIdList)
		            .eq("enterprise_id", enterpriseId).eq("deleted", Status.FALSE.getKey()));
		    if (!org.springframework.util.CollectionUtils.isEmpty(list)) {
		        userIds = list.stream().map(ue -> ue.getUserId()).collect(Collectors.toSet());
		    }
		} catch (Exception e) {
            log.error("query error", e);
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED, null);
        }
        
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userIds);
	}
}

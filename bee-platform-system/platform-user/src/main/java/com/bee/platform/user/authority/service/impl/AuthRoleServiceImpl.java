package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dao.mapper.SystemCodeMapper;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.SystemCode;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.CommonUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.user.authority.dao.mapper.*;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.entity.*;
import com.bee.platform.user.authority.rq.*;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthResourceService;
import com.bee.platform.user.authority.service.AuthRoleService;
import com.bee.platform.user.authority.service.AuthUsergroupService;
import com.bee.platform.user.enums.AppAbbreviationType;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
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
 * 角色表 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthRoleServiceImpl extends ServiceImpl<AuthRoleMapper, AuthRole> implements AuthRoleService {

    @Autowired
    private AuthRoleMapper authRoleMapper;
    @Autowired
    private AuthRoleResourceMapper roleResourceMapper;
    @Autowired
    private AuthRoleInterfaceMapper roleInterfaceMapper;
    @Autowired
    private AuthFunctionRoleMapper functionRoleMapper;
    @Autowired
    private SystemCodeMapper systemCodeMapper;
    @Autowired
    private AuthResourceMapper resourceMapper;
    @Autowired
    private AuthInterfaceMapper interfaceMapper;
    @Autowired
    private AuthUserRoleMapper userRoleMapper;
    @Autowired
    private AuthResourceService resourceService;
    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private AuthUsergroupService usergroupService;
    @Autowired
    private AuthUsergroupRoleMapper usergroupRoleMapper;

    private static Integer ZERO = 0;
    private static Integer ONE = 1;

    /**
     * 获取角色列表-后台
     * @param pagination
     * @return
     */
    @Override
    public List<AuthRoleDTO> listRolesBack(Pagination pagination, AuthRoleRQ roleRQ) {
        Map<String,Object> map = new HashMap<>(16);
        if (!StringUtils.isEmpty(roleRQ.getRoleType())){
            map.put("roleType",roleRQ.getRoleType());
        }
        if (!StringUtils.isEmpty(roleRQ.getRoleName())){
            map.put("roleName",roleRQ.getRoleName());
        }
        if (!StringUtils.isEmpty(roleRQ.getSysType())){
            map.put("sysType",roleRQ.getSysType());
        }
        if (!StringUtils.isEmpty(roleRQ.getCreateStartTime()) && DateUtils.isValidDate(roleRQ.getCreateStartTime())){
            map.put("createStartTime",roleRQ.getCreateStartTime()  + " 00:00:00");
        }
        if (!StringUtils.isEmpty(roleRQ.getCreateEndTime()) && DateUtils.isValidDate(roleRQ.getCreateEndTime())){
            map.put("createEndTime",roleRQ.getCreateEndTime()  + " 23:59:59");
        }
        List<AuthRole> roles = authRoleMapper.selectRolesByCondition(map,pagination);
        List<AuthRoleDTO> rolesDTOs = BeanUtils.assemble(AuthRoleDTO.class,roles);
        for (AuthRoleDTO rolesDTO : rolesDTOs){
            rolesDTO.setRelatedNumber(userRoleMapper.selectCount(new EntityWrapper<AuthUserRole>()
                    .eq("role_id",rolesDTO.getId()).and()
                    .eq("deleted",Status.FALSE.getKey())));
        }
        return rolesDTOs;
    }

    /**
     * 添加/修改角色-后台
     * @param userInfo
     * @param rq
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addRolesBack(AuthPlatformUserInfo userInfo, AuthRoleAddRQ rq) {
        AuthRole role = new AuthRole()
                .setRoleName(rq.getRoleName())
                .setRoleType(rq.getRoleType())
                .setSubSys(rq.getSubSys())
                .setLevel(rq.getLevel())
                .setDeleted(Status.FALSE.getKey());
        if (!ObjectUtils.isEmpty(rq.getId())){
            // 更新角色
            if (authRoleMapper.updateById(role.setId(Integer.valueOf(rq.getId())).setUpdateTime(new Date())) < ZERO){
                log.error("修改角色失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
        }else {
            // 插入角色
            if (authRoleMapper.insert(role.setCreateUser(userInfo.getId()).setCreateTime(new Date())) <= ZERO){
                log.error("增加角色失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
        }
        if (EnumRoleType.BASE.getCode().equals(rq.getRoleType())){
            // 删除菜单角色关系
            if (roleResourceMapper.delete(new EntityWrapper<AuthRoleResource>().eq("role_id",role.getId())) < ZERO){
                log.error("删除菜单角色关系失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
            // 删除接口角色关系
            if (roleInterfaceMapper.delete(new EntityWrapper<AuthRoleInterface>().eq("role_id",role.getId())) < ZERO){
                log.error("删除接口角色关系失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
            List<AuthResource> resourcesNew = this.getParentResource(rq.getResourceRQS());

            // 角色菜单关联
            // for (AuthResourceRQ r : rq.getResourceRQS()){
            for (AuthResource r : resourcesNew){
                AuthRoleResource authResource = new AuthRoleResource()
                        .setRoleId(role.getId())
                        .setResourceId(r.getId())
                        .setDeleted(Status.FALSE.getKey())
                        .setCreateUser(userInfo.getId())
                        .setCreateTime(new Date());
                if (roleResourceMapper.selectCount(new EntityWrapper<>(new AuthRoleResource()
                        .setDeleted(Status.FALSE.getKey()).setRoleId(role.getId()).setResourceId(r.getId()))) > ZERO){
                    continue;
                }
                if (roleResourceMapper.insert(authResource) <= ZERO){
                    log.error("保存菜单角色关系失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                    throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
                }
            }
            // 角色接口关联
            for (AuthInterfaceDetailRQ r : rq.getInterfaceDetailRQS()){
                AuthRoleInterface authInterface = new AuthRoleInterface()
                        .setRoleId(role.getId())
                        .setInterfaceId(r.getId())
                        .setDeleted(Status.FALSE.getKey())
                        .setCreateUser(userInfo.getId())
                        .setCreateTime(new Date());
                if (roleInterfaceMapper.insert(authInterface) <= ZERO){
                    log.error("保存接口角色关系失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                    throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
                }
            }
        }else {
            // 删除功能角色关系
            if (functionRoleMapper.delete(new EntityWrapper<AuthFunctionRole>().eq("pid",role.getId())) < ZERO){
                log.error("删除功能角色失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
            }
            if (CollectionUtils.isEmpty(rq.getRoleAddRQS())){
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }
            // 基础角色关联
            for (AuthRoleAddRQ r : rq.getRoleAddRQS()){
                AuthFunctionRole functionRole = new AuthFunctionRole()
                        .setPid(role.getId())
                        .setRoleId(Integer.valueOf(r.getId()))
                        .setDeleted(Status.FALSE.getKey())
                        .setCreateUser(userInfo.getId())
                        .setCreateTime(new Date());
                if (functionRoleMapper.insert(functionRole) <= ZERO){
                    log.error("功能基础角色关系失败,调用{}类{}方法出错","AuthRoleServiceImpl","addAppRolesBack()");
                    throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
                }
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 获取父级菜单并且排序
     * @param resourceRQS
     * @return
     */
    private List<AuthResource> getParentResource(List<AuthResourceRQ> resourceRQS) {
        List<AuthResource> resources = BeanUtils.assemble(AuthResource.class,resourceRQS);
        for (AuthResourceRQ r : resourceRQS){
            AuthResource resource = resourceMapper.selectById(new AuthResource().setId(r.getPid()));
            if (!ObjectUtils.isEmpty(resource)){
                resources.add(resource);
                if (!ZERO.equals(resource.getPid())){
                    AuthResource resource1 = resourceMapper.selectById(new AuthResource().setId(resource.getPid()));
                    resources.add(resource1);
                }
            }
        }
        /*Set<AuthResource> set = new HashSet();
        set.addAll(resources);
        List<AuthResource> resourcesNew = new ArrayList<>();
        resourcesNew.addAll(set);
        //
        Collections.sort(resourcesNew, new Comparator<AuthResource>() {
            @Override
            public int compare(AuthResource o1, AuthResource o2) {
                if (o1.getOrderNum() == null) {
                    return 1;
                } else if (o2.getOrderNum() == null) {
                    return 0;
                }else {
                    return o1.getOrderNum().compareTo(o2.getOrderNum());
                }
            }
        });*/
        return resources;
    }

    /**
     * 获取角色-不分页
     * @param
     * @return
     */
    @Override
    public List<AuthRoleDTO> listRolesBackNoPage(RoleQueryRQ rq) {
        Wrapper<AuthRole> wrapper = new EntityWrapper<AuthRole>()
                .eq("deleted",Status.FALSE.getKey())
                .orderBy("create_time",true);
        if (!ObjectUtils.isEmpty(rq) && !StringUtils.isEmpty(rq.getSubType())){
            wrapper.and().eq("sub_sys",rq.getSubType());
        }
        if (!ObjectUtils.isEmpty(rq) && !StringUtils.isEmpty(rq.getRoleType())){
            if (EnumRoleType.APPLICATION.getCode().equals(rq.getRoleType())){
                wrapper.and().eq("role_type",EnumRoleType.FUNCTION_ONE.getCode());
            }else if (EnumRoleType.FUNCTION_ONE.getCode().equals(rq.getRoleType())){
                wrapper.and().eq("role_type",EnumRoleType.FUNCTION_TWO.getCode());
            }else {
                wrapper.and().eq("role_type",EnumRoleType.BASE.getCode());
            }
        }
        return BeanUtils.assemble(AuthRoleDTO.class,authRoleMapper.selectList(wrapper));
    }

    /**
     * 删除角色
     * @param id
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteRole(AuthPlatformUserInfo userInfo, String id) {
        if (authRoleMapper.updateById(new AuthRole()
                .setId(Integer.valueOf(id))
                .setDeleted(Status.TRUE.getKey())
                .setUpdateTime(new Date())) <= ZERO){
            log.error("删除角色失败,调用{}类{}方法出错","AuthRoleServiceImpl","deleteRole()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        if (functionRoleMapper.update(new AuthFunctionRole().setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<AuthFunctionRole>().eq("pid",id)) < ZERO){
            log.error("删除角色关系失败,调用{}类{}方法出错","AuthRoleServiceImpl","deleteRole()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 获取角色码表
     * @return
     */
    @Override
    public ResponseResult<List<SystemCodeDTO>> listRoleSystemCode() {
        List<SystemCode> list = systemCodeMapper.selectList(new EntityWrapper<SystemCode>().eq("sys_group_id","role_type"));
        List<SystemCodeDTO> dtoList = new ArrayList<SystemCodeDTO>();

        for (SystemCode s : list) {
            SystemCodeDTO systemCodeDTO = new SystemCodeDTO()
                    .setSysGroupId(s.getSysGroupId())
                    .setSysCode(s.getSysCode())
                    .setSysCodeDesc(s.getSysCodeDesc());
            if (EnumRoleType.APPLICATION.getCode().equals(s.getSysCode())){
                systemCodeDTO.setLevel(1);
                dtoList.add(systemCodeDTO);
            }else if (EnumRoleType.FUNCTION_ONE.getCode().equals(s.getSysCode())){
                systemCodeDTO.setLevel(2);
                dtoList.add(systemCodeDTO);
            }else if (EnumRoleType.FUNCTION_TWO.getCode().equals(s.getSysCode())){
                systemCodeDTO.setLevel(3);
                dtoList.add(systemCodeDTO);
            }else if (EnumRoleType.BASE.getCode().equals(s.getSysCode())){
                systemCodeDTO.setLevel(4);
                dtoList.add(systemCodeDTO);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtoList);
    }

    /**
     * 获取角色详细信息
     * @param id
     * @return
     */
    @Override
    public ResponseResult<AuthRoleDetailDTO> getRoleDetail(String id) {
        AuthRole role = authRoleMapper.selectById(new AuthRole()
                .setId(Integer.valueOf(id)).setDeleted(Status.FALSE.getKey()));
        AuthRoleDetailDTO roleDetailDTO = new AuthRoleDetailDTO();
        BeanUtils.copyProperties(role,roleDetailDTO);

        if (EnumRoleType.BASE.getCode().equals(role.getRoleType())){
            // 所有菜单
            List<AuthResource> resources = resourceMapper.selectList(new EntityWrapper<AuthResource>()
                    .eq("sub_sys",role.getSubSys()).and()
                    .eq("deleted",Status.FALSE.getKey()));
            List<AuthResourceDetailDTO> resourceDetailDTOS = BeanUtils.assemble(AuthResourceDetailDTO.class, resources);
            // 所有接口
            List<AuthInterface> interfaces = interfaceMapper.selectList(new EntityWrapper<AuthInterface>()
                    .eq("sub_sys",role.getSubSys()).and()
                    .eq("deleted",Status.FALSE.getKey()));
            List<AuthInterfaceDto> authInterfaceDtos = BeanUtils.assemble(AuthInterfaceDto.class, interfaces);

            for (AuthResourceDetailDTO resource : resourceDetailDTOS) {
                if (ObjectUtils.isEmpty(roleResourceMapper.selectOne(new AuthRoleResource()
                        .setRoleId(role.getId()).setResourceId(resource.getId()).setDeleted(Status.FALSE.getKey())))){
                    resource.setOpenStatu(false);
                }else {
                    resource.setOpenStatu(true);
                }
            }
            for (AuthInterfaceDto inter : authInterfaceDtos){
                if (ObjectUtils.isEmpty(roleInterfaceMapper.selectOne(new AuthRoleInterface()
                        .setRoleId(role.getId()).setInterfaceId(inter.getId()).setDeleted(Status.FALSE.getKey())))){
                    inter.setOpenStatu(false);
                }else {
                    inter.setOpenStatu(true);
                }
            }
            roleDetailDTO.setResourceDetail(resourceService.getResourcesTree(resourceDetailDTOS));
            roleDetailDTO.setInterfaceDetail(authInterfaceDtos);
        }else {
            List<AuthRole> roles = authRoleMapper.selectList(new EntityWrapper<AuthRole>()
                    .eq("level",role.getLevel() + 1).and()
                    .eq("deleted",Status.FALSE.getKey()).and()
                    .eq("sub_sys",role.getSubSys()));
            List<AuthRoleDTO> roleDTOList = BeanUtils.assemble(AuthRoleDTO.class,roles);
            for (AuthRoleDTO roleDTO : roleDTOList){
                if (ObjectUtils.isEmpty(functionRoleMapper.selectOne(new AuthFunctionRole()
                        .setPid(role.getId()).setRoleId(roleDTO.getId())))){
                    roleDTO.setOpenStatu(false);
                }else {
                    roleDTO.setOpenStatu(true);
                }
            }
            roleDetailDTO.setRoleDetail(roleDTOList);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,roleDetailDTO);
    }

    /**
     * 获取子系统详细信息
     * @return
     */
    @Override
    public ResponseResult<List<SystemCodeDTO>> listSubSystemCode() {
        List<SystemCode> list = systemCodeMapper.selectList(new EntityWrapper<SystemCode>()
            .eq("status", 1)
            .eq("sys_group_id","sub_system"));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,BeanUtils.assemble(SystemCodeDTO.class,list));
    }

    /**
     *
     * @param id
     * @return
     */
    @Override
    public List<AuthRoleDTO> listSubRolesBackNoPage(String id) {
        return functionRoleMapper.listSubRolesBackNoPage(id);
    }

    /**
     * 获取已开通产品
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<AuthAppRoleDTO>> getAppRole(AuthPlatformUserInfo userInfo) {
        List<AuthRole> userRoles = Lists.newArrayList();
        List<AuthAppRoleDTO> appRoles = Lists.newArrayList();
        List<AuthRole> orgRoles = authRoleMapper.selectAppRoleByOrg(userInfo.getOrgId());
        // 用户为企业管理员，直接返回企业所有角色
        if (userService.isManager(userInfo.getOrgId(),userInfo.getId(),EnumRoleType.ENTERPRISE_ADMIN.getCode())){
            appRoles = BeanUtils.assemble(AuthAppRoleDTO.class,orgRoles);
        }else {
            //userRoles = authRoleMapper.selectAppRoleByUser(userInfo.getId(),userInfo.getOrgId());
            userRoles = this.getUserApp(userInfo);
            //userRoles = authRoleMapper.selectUserApp(userInfo.getId(),userInfo.getOrgId());
            // 查询用户组下应用角色
            List<AuthRole> groupAppRoles = this.getGroupAppRoles(userInfo);
            // 用户角色和用户组角色做并集
            List<AuthRole> tempRoles = CommonUtils.getUnion(userRoles,groupAppRoles);
            // 与企业角色取交集
            appRoles = BeanUtils.assemble(AuthAppRoleDTO.class,CommonUtils.getIntersection(tempRoles,orgRoles));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,this.setAbbreviation(appRoles));
    }

    /**
     * 递归查询应用角色
     * @param userInfo
     * @return
     */
    private List<AuthRole> getUserApp(AuthPlatformUserInfo userInfo) {
        // 查询基础角色
        List<AuthUserRole> base = userRoleMapper.selectList(new EntityWrapper<>(new AuthUserRole()
                .setUserId(userInfo.getId())
                .setEnterpriseId(userInfo.getOrgId())
                .setRoleType(EnumRoleType.BASE.getCode())
                .setDeleted(Status.FALSE.getKey())));
        if (CollectionUtils.isEmpty(base)){
            return Lists.newArrayList();
        }
        // 查询功能二级角色
        List<AuthFunctionRole> funcTwo = functionRoleMapper.selectList(new EntityWrapper<AuthFunctionRole>()
                .eq("deleted",Status.FALSE.getKey())
                .in("role_id",base.stream().map(a->a.getRoleId()).collect(Collectors.toList())));
        if (CollectionUtils.isEmpty(funcTwo)){
            return Lists.newArrayList();
        }
        // 查询功能一级角色
        List<AuthFunctionRole> funcOne = functionRoleMapper.selectList(new EntityWrapper<AuthFunctionRole>()
                .eq("deleted",Status.FALSE.getKey())
                .in("role_id",funcTwo.stream().map(a->a.getPid()).collect(Collectors.toList())));
        if (CollectionUtils.isEmpty(funcOne)){
            return Lists.newArrayList();
        }
        // 查询应用角色
        List<AuthFunctionRole> app = functionRoleMapper.selectList(new EntityWrapper<AuthFunctionRole>()
                .eq("deleted",Status.FALSE.getKey())
                .in("role_id",funcOne.stream().map(a->a.getPid()).collect(Collectors.toList())));
        if (CollectionUtils.isEmpty(app)){
            return Lists.newArrayList();
        }
        List<AuthRole> list = authRoleMapper.selectList(new EntityWrapper<AuthRole>()
                .eq("role_type",EnumRoleType.APPLICATION.getCode())
                .eq("deleted",Status.FALSE.getKey())
                .in("id",app.stream().map(a->a.getPid()).collect(Collectors.toList())));
        return list;
    }

    /**
     * 取角色并集
     * @param list1
     * @param list2
     * @return
     */
    /*private List<AuthRole> getUnionRoles(List<AuthRole> list1, List<AuthRole> list2) {
        List<AuthRole> result = Lists.newArrayList();
        Set<AuthRole> set = new HashSet();
        set.addAll(list1);
        set.addAll(list2);
        result.addAll(set);
        return result;
    }*/

    /**
     * 获取用户用户组下应用角色
     * @param userInfo
     * @return
     */
    private List<AuthRole> getGroupAppRoles(AuthPlatformUserInfo userInfo) {
        // 查询用户所在用户组
        List<AuthUsergroup> usergroups = usergroupService.getByEnterpriseAndUserId(userInfo.getOrgId(), userInfo.getId());
        if (CollectionUtils.isEmpty(usergroups)){
            return Lists.newArrayList();
        }
        // 查询用户组下应用角色
        List<AuthUsergroupRole> list = usergroupRoleMapper.selectList(new EntityWrapper<AuthUsergroupRole>()
                    .eq("enterprise_id",userInfo.getOrgId())
                    .eq("deleted",Status.FALSE.getKey())
                    .in("usergroup_id",usergroups.stream().map(a -> a.getId()).collect(Collectors.toList())));
        // 查询用户组下应用角色
        if (CollectionUtils.isEmpty(list)){
            return Lists.newArrayList();
        }
        List<AuthRole> groupAppRoles = authRoleMapper.selectList(new EntityWrapper<AuthRole>()
                    .eq("deleted",Status.FALSE.getKey())
                    .eq("role_type",EnumRoleType.APPLICATION.getCode())
                    .in("id",list.stream().map(a->a.getRoleId()).collect(Collectors.toList())));
        return groupAppRoles;
    }

    /**
     * 获取未开通产品
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<AuthAppRoleDTO>> getNotOpenAppRole(AuthPlatformUserInfo userInfo) {
        // 获取所有应用
        List<AuthRole> allApps = authRoleMapper.selectList(new EntityWrapper<AuthRole>()
                .eq("deleted",Status.FALSE.getKey())
                .eq("role_type",EnumRoleType.APPLICATION.getCode())
                .ne("sub_sys","bee_platform")
                .ne("sub_sys","bee_console"));
        List<AuthAppRoleDTO> allAppDTO = BeanUtils.assemble(AuthAppRoleDTO.class,allApps);
        // 获取用户已开通应用
        List<AuthAppRoleDTO> appRoles = (List<AuthAppRoleDTO>)this.getAppRole(userInfo).getObject();

        appRoles.stream().map(obj->obj.setAbbreviation(null)).collect(Collectors.toList());
        allAppDTO.removeAll(appRoles);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,this.setAbbreviation(allAppDTO));
    }

    /**
     * 取交集
     * @param userRoles
     * @param orgRoles
     * @return
     */
    /*private List<AuthAppRoleDTO> getIntersection(List<AuthRole> userRoles, List<AuthRole> orgRoles) {
        List<AuthRole> roles = Lists.newArrayList();
        Set<AuthRole> set = new HashSet();
        set = Sets.intersection(Sets.newHashSet(userRoles), Sets.newHashSet(orgRoles)).copyInto(set);
        roles.addAll(set);
        return BeanUtils.assemble(AuthAppRoleDTO.class,roles);
    }*/

    /**
     * 设置缩写
     * @param appRoles
     * @return
     */
    private List<AuthAppRoleDTO> setAbbreviation(List<AuthAppRoleDTO> appRoles) {
        for (AuthAppRoleDTO app : appRoles) {
            if ("bee_platform".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.PLAT.getCode());
            }
            if ("bee_supply_chain_finance".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.SCF.getCode());
            }
            if ("bee_trade".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.TRD.getCode());
            }
            if ("bee_iot".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.IIOT.getCode());
            }
            if ("bee_logistics".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.WL.getCode());
            }
            if ("bee_oa".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.EKP.getCode());
            }
            if ("bee_industrial_brain".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.ERP.getCode());
            }
            if ("bee_industrial_brain_si".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.ERPSI.getCode());
            }
            if ("wly_supply_chain_finance".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.WSCF.getCode());
            }
            if ("bee_industrial_brain_dinas".equals(app.getSubSys())){
                app.setAbbreviation(AppAbbreviationType.ERPDINAS.getCode());
            }
        }
        return appRoles;
    }


    @Override
    public List<AuthBackRoleTreeDTO> getBackRoleTree() {
        // 查询所有后台的 application
        List<AuthBackRoleTreeDTO> application =  baseMapper.getBackApplication();
        if(CollectionUtils.isEmpty(application)){
            return Lists.newArrayList();
        }
        // 获取系统标识列表
        List<String> appSubSys = application.stream().map(o->o.getSubSys()).distinct().collect(Collectors.toList());
        // 查询所有子集
        List<AuthBackRoleTreeDTO> allChildRole = baseMapper.getAllChildRole(appSubSys);

        List<AuthBackRoleTreeDTO> total = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(application)) {
            total.addAll(application);
        }
        if (!CollectionUtils.isEmpty(allChildRole)) {
            total.addAll(allChildRole);
        }

        return buildAllRoleTree(total);
    }




    public static List<AuthBackRoleTreeDTO> buildAllRoleTree(List<AuthBackRoleTreeDTO> treeNodes) {

        List<AuthBackRoleTreeDTO> trees = new ArrayList<>();
        for (AuthBackRoleTreeDTO treeNode : treeNodes) {
            if (!ObjectUtils.isEmpty(treeNode.getPid()) && 0 == (treeNode.getPid())) {
                trees.add(treeNode);
            }
            for (AuthBackRoleTreeDTO it : treeNodes) {
                if (treeNode.getRoleId().equals(it.getPid())) {
                    if (treeNode.getChildren() == null) {
                        treeNode.setChildren(new ArrayList<>());
                    }
                    treeNode.getChildren().add(it);
                }
            }
        }
        return trees;
    }
}

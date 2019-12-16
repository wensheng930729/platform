package com.bee.platform.user.authority.service.impl;

import cn.hutool.poi.excel.ExcelUtil;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.AuthUserRoleInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.CommonUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.user.authority.dao.mapper.*;
import com.bee.platform.user.authority.dto.AuthResourceDetailDTO;
import com.bee.platform.user.authority.entity.*;
import com.bee.platform.user.authority.enums.EnumLevel;
import com.bee.platform.user.authority.rq.AuthResourceQueryRQ;
import com.bee.platform.user.authority.rq.AuthResourceRQ;
import com.bee.platform.user.authority.service.*;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 资源表 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthResourceServiceImpl extends ServiceImpl<AuthResourceMapper, AuthResource> implements AuthResourceService {

    @Autowired
    private AuthResourceMapper resourceMapper;
    @Autowired
    private AuthUserRoleMapper userRoleMapper;
    @Autowired
    private AuthEnterpriseRoleMapper enterpriseRoleMapper;
    @Autowired
    private AuthFunctionRoleMapper functionRoleMapper;
    @Autowired
    private AuthRoleService authRoleService;
    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private AuthUsergroupService usergroupService;
    @Autowired
    private AuthUsergroupRoleMapper usergroupRoleMapper;
    @Autowired
    private AuthUserRoleBackMapper userRoleBackMapper;
    @Autowired
    private AuthUsergroupBackService usergroupBackService;
    @Autowired
    private AuthUsergroupRoleBackService usergroupRoleBackService;

    private static Integer ZERO = 0;
    private static Integer TWO = 2;
    /**
     * 获得所有资源-后台（分页）
     * @param pagination
     * @return
     */
    @Override
    public List<AuthResourceDetailDTO> listResourcesWithPage(AuthResourceQueryRQ rq, Pagination pagination) {
        Map<String,Object> map = new HashMap<>();
        if (!StringUtils.isEmpty(rq.getResourceName())){
            map.put("resourceName",rq.getResourceName());
        }
        if (!StringUtils.isEmpty(rq.getSubSystemType())){
            map.put("subSystemType",rq.getSubSystemType());
        }
        if (!StringUtils.isEmpty(rq.getCreateStartTime()) && DateUtils.isValidDate(rq.getCreateStartTime())){
            map.put("createStartTime",rq.getCreateStartTime()  + " 00:00:00");
        }
        if (!StringUtils.isEmpty(rq.getCreateEndTime()) && DateUtils.isValidDate(rq.getCreateEndTime())){
            map.put("createEndTime",rq.getCreateEndTime()  + " 23:59:59");
        }
        List<AuthResource> resources = resourceMapper.selectResourcesByCondition(map, pagination);
        return BeanUtils.assemble(AuthResourceDetailDTO.class,resources);
    }

    /**
     * 菜单树
     * @param resourcesList
     * @return
     */
    @Override
    public List<AuthResourceDetailDTO> getResourcesTree(List<AuthResourceDetailDTO> resourcesList) {
        List<AuthResourceDetailDTO> resources = new ArrayList<>();
        Map<Integer, AuthResourceDetailDTO> resourceMap = new HashMap<Integer, AuthResourceDetailDTO>();
        if (CollectionUtils.isEmpty(resourcesList)){
            return resources;
        }
        //
        Collections.sort(resourcesList, new Comparator<AuthResourceDetailDTO>() {
            @Override
            public int compare(AuthResourceDetailDTO o1, AuthResourceDetailDTO o2) {
                if (o1.getOrderNum() == null) {
                    return 1;
                } else if (o2.getOrderNum() == null) {
                    return 0;
                }else {
                    return o1.getOrderNum().compareTo(o2.getOrderNum());
                }
            }
        });
        for (AuthResourceDetailDTO r : resourcesList){
            resourceMap.put(r.getId(),r);
        }
        for (AuthResourceDetailDTO r : resourcesList){
            AuthResourceDetailDTO child = r;
            if (ZERO.equals(child.getPid())){
                resources.add(r);
            }else {
                AuthResourceDetailDTO parent = resourceMap.get(child.getPid());
                if(parent!=null){
                    if (CollectionUtils.isEmpty(parent.getRoutes())){
                        parent.setRoutes(new ArrayList<>());
                    }
                    parent.getRoutes().add(child);
                }
            }
        }
        return resources;
    }

    /**
     * 获取资源列表-用户
     * @param userInfo
     * @return
     */
    /*@Override
    public List<AuthResourceDetailDTO> listResourcesByUser(AuthPlatformUserInfo userInfo, String subSys) {
        List<AuthResourceDetailDTO> resourceDetailOrg = new ArrayList<>();
        List<AuthResourceDetailDTO> resourceDetailUser = new ArrayList<>();
        List<AuthResourceDetailDTO> resourceDetail = new ArrayList<>();
        String s = "";
        //查询用户在当前企业中的角色
        List<AuthUserRole> authUserRoles = userRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                .eq("user_id", userInfo.getId())
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("deleted", Status.FALSE.getKey()));
        List<AuthFunctionRole> baseList = new ArrayList<>();
        // 查询企业对应的二级功能角色
        List<AuthEnterpriseRole> list1 = enterpriseRoleMapper.selectList(new EntityWrapper<AuthEnterpriseRole>()
                .eq("enterprise_id",userInfo.getOrgId()).and()
                .eq("level", EnumLevel.Level.function_two.getKey()).and()
                .eq("deleted",Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(list1)){
            // 根据二级功能查询所有基础功能
            baseList = functionRoleMapper.selectByFuncionTwo(list1);
        }else {
            baseList = functionRoleMapper.selectByFuncionTwoUser(authUserRoles);
        }
        // 根据基础角色查询所有菜单
        if (!CollectionUtils.isEmpty(baseList)){
            resourceDetailOrg = resourceMapper.selectResourcesByOrgRoles(baseList,subSys).stream()
                    .filter(r->ZERO.equals(r.getShowType()) ||
                    TWO.equals(r.getShowType())).map(obj->{
                return BeanUtils.copyProperties(obj,AuthResourceDetailDTO.class).setHideChildrenMenu(isHideChildrenMenu(obj.getHide()));
            }).collect(Collectors.toList());
        }
        for (AuthUserRole role : authUserRoles){
            s = s + "," + role.getRoleType();
        }
        if (!Arrays.asList(s.split(",")).contains(EnumRoleType.ENTERPRISE_ADMIN.getCode())){
            List<AuthUserRole> list = userRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                    .eq("user_id",userInfo.getId()).and()
                    .eq("enterprise_id",userInfo.getOrgId()).and()
                    .eq("level",EnumLevel.Level.basic.getKey()).and()
                    .eq("deleted",Status.FALSE.getKey()));
            if (!CollectionUtils.isEmpty(list)) {
                resourceDetailUser = resourceMapper.selectResourcesByUserRoles(list, subSys).stream().filter(r->TWO.compareTo(r.getShowType()) > ZERO )
                        .map(obj -> BeanUtils.copyProperties(obj, AuthResourceDetailDTO.class)
                                .setHideChildrenMenu(isHideChildrenMenu(obj.getHide()))
                        )
                        .collect(Collectors.toList());
            }
            Set<AuthResourceDetailDTO> set = new HashSet();
            set = Sets.intersection(Sets.newHashSet(resourceDetailUser), Sets.newHashSet(resourceDetailOrg)).copyInto(set);
            resourceDetail.addAll(set);
            return this.getResourcesTree(resourceDetail);
        }
        return this.getResourcesTree(resourceDetailOrg.stream().filter(r->ZERO.equals(r.getShowType()) ||
                TWO.equals(r.getShowType())).collect(Collectors.toList()));
    }*/
    @Override
    public List<AuthResourceDetailDTO> listResourcesByUser(AuthPlatformUserInfo userInfo, String subSys) {
        List<AuthResourceDetailDTO> resourceDetailOrg = Lists.newArrayList();
        // 查询企业对应的二级功能角色
        List<AuthEnterpriseRole> enterpriseRoles = enterpriseRoleMapper.selectList(new EntityWrapper<AuthEnterpriseRole>()
                .eq("enterprise_id",userInfo.getOrgId()).and()
                .eq("level", EnumLevel.Level.function_two.getKey()).and()
                .eq("deleted",Status.FALSE.getKey()));
        // 企业权限为空
        if (CollectionUtils.isEmpty(enterpriseRoles)){
            return Lists.newArrayList();
        }
        // 用户为企业管理员，直接返回企业所有角色
        if (userService.isManager(userInfo.getOrgId(),userInfo.getId(),EnumRoleType.ENTERPRISE_ADMIN.getCode())){
            // 企业对应基础角色
            List<AuthFunctionRole> enterpriseRolesList = functionRoleMapper.selectByFuncionTwo(enterpriseRoles);
            if (CollectionUtils.isEmpty(enterpriseRoles)){
                return Lists.newArrayList();
            }
            // 查询所有菜单
            resourceDetailOrg = resourceMapper.selectResourcesByOrgRoles(enterpriseRolesList,subSys).stream().map(obj->{
                return BeanUtils.copyProperties(obj,AuthResourceDetailDTO.class).setHideChildrenMenu(isHideChildrenMenu(obj.getHide()));
            }).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(resourceDetailOrg)){
                return Lists.newArrayList();
            }
            // 构造菜单树
            return this.getResourcesTree(resourceDetailOrg.stream().filter(r->ZERO.equals(r.getShowType()) ||
                    TWO.equals(r.getShowType())).collect(Collectors.toList()));
        }
        // 直接获取用户基础角色
        List<AuthFunctionRole> directList = this.getBaseRolesDirect(userInfo);
        // 根据用户组查询基础角色
        List<AuthFunctionRole> userGroupList = this.getBaseRolesDirectFromUserGroup(userInfo);
        // 取并集
        List<AuthFunctionRole> baseList = CommonUtils.getUnion(directList,userGroupList);
        // 根据基础角色查询所有菜单
        if (!CollectionUtils.isEmpty(baseList)){
            resourceDetailOrg = resourceMapper.selectResourcesByOrgRoles(baseList,subSys).stream().map(obj->{
                return BeanUtils.copyProperties(obj,AuthResourceDetailDTO.class).setHideChildrenMenu(isHideChildrenMenu(obj.getHide()));
            }).collect(Collectors.toList());
        }
        return this.getResourcesTree(resourceDetailOrg.stream().filter(r->TWO.compareTo(r.getShowType()) > ZERO ).collect(Collectors.toList()));
    }

    /**
     * 从用户组查询用户基础角色
     * @param userInfo
     * @return
     */
    private List<AuthFunctionRole> getBaseRolesDirectFromUserGroup(AuthPlatformUserInfo userInfo) {
        List<AuthFunctionRole> baseList = Lists.newArrayList();
        // 查询用户所在用户组
        List<AuthUsergroup> usergroups = usergroupService.getByEnterpriseAndUserId(userInfo.getOrgId(), userInfo.getId());
        if (CollectionUtils.isEmpty(usergroups)){
            return Lists.newArrayList();
        }
        // 查询用户组对应角色
        List<AuthUsergroupRole> authUsergroupRoles = usergroupRoleMapper.selectList(new EntityWrapper<AuthUsergroupRole>()
                .eq("enterprise_id",userInfo.getOrgId())
                .eq("deleted",Status.FALSE.getKey())
                .in("usergroup_id",usergroups.stream().map(a -> a.getId()).collect(Collectors.toList())));
        if (CollectionUtils.isEmpty(authUsergroupRoles)){
            return Lists.newArrayList();
        }
        // 根据用户组查询对应角色
        List<AuthFunctionRole> groupRoleList = functionRoleMapper.selectByUserGroup(authUsergroupRoles);
        // 查询企业对应的二级功能角色
        List<AuthEnterpriseRole> enterpriseRoles = enterpriseRoleMapper.selectList(new EntityWrapper<AuthEnterpriseRole>()
                .eq("enterprise_id",userInfo.getOrgId()).and()
                .eq("level", EnumLevel.Level.function_two.getKey()).and()
                .eq("deleted",Status.FALSE.getKey()));
        List<AuthFunctionRole> enterpriseRolesList = functionRoleMapper.selectByFuncionTwo(enterpriseRoles);
        // 去重
        Set<AuthFunctionRole> set = new HashSet();
        set = Sets.intersection(Sets.newHashSet(enterpriseRolesList), Sets.newHashSet(groupRoleList)).copyInto(set);
        baseList.addAll(set);
        return baseList;
    }

    /**
     * 直接查询用户基础角色
     * @param userInfo
     * @return
     */
    private List<AuthFunctionRole> getBaseRolesDirect(AuthPlatformUserInfo userInfo) {
        List<AuthFunctionRole> baseList = Lists.newArrayList();
        //查询用户在当前企业中的基础角色
        List<AuthUserRole> authUserRoles = userRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                .eq("user_id", userInfo.getId())
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("deleted", Status.FALSE.getKey())
                .eq("level",EnumLevel.Level.basic.getKey()));
        if (CollectionUtils.isEmpty(authUserRoles)){
            return baseList;
        }
        List<AuthFunctionRole> enterpriseRolesList = Lists.newArrayList();
        // 查询企业对应的二级功能角色
        List<AuthEnterpriseRole> enterpriseRoles = enterpriseRoleMapper.selectList(new EntityWrapper<AuthEnterpriseRole>()
                .eq("enterprise_id",userInfo.getOrgId()).and()
                .eq("level", EnumLevel.Level.function_two.getKey()).and()
                .eq("deleted",Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(enterpriseRoles)){
            // 根据二级功能查询所有基础功能
            enterpriseRolesList = functionRoleMapper.selectByFuncionTwo(enterpriseRoles);
        }else {
            // 企业未授权
            return Lists.newArrayList();
        }
        // 去重
        List<AuthFunctionRole> userRolesList = functionRoleMapper.selectByFuncionTwoUser(authUserRoles);
        Set<AuthFunctionRole> set = new HashSet();
        set = Sets.intersection(Sets.newHashSet(enterpriseRolesList), Sets.newHashSet(userRolesList)).copyInto(set);
        baseList.addAll(set);

        return baseList;
    }

    /**
     * @notes: 根据用户的子系统标识和角色id查询角色可访问的菜单
     * @Author: junyang.li
     * @Date: 11:15 2019/6/14
     * @param subSys : 子系统标识
     * @param roleId : 角色id
     * @param userInfo : 登录凭证
     * @return: java.util.List<com.bee.platform.user.authority.dto.AuthResourceDetailDTO>
     */
    /*@Override
    public List<AuthResourceDetailDTO> getUserResourceByRoleId(String subSys, Integer roleId, AuthPlatformUserInfo userInfo) {
        List<AuthResourceDetailDTO> list= Lists.newArrayList();
        //查询用户在当前企业中的角色
        List<AuthUserRole> authUserRoles = userRoleMapper.getUserRoleIdBySubSys(userInfo.getOrgId(),userInfo.getId(),subSys);
        //判空
        if(CollectionUtils.isEmpty(authUserRoles)){
            return list;
        }
        AuthRole authRole=authRoleService.selectById(roleId);
        if(authRole==null || !authRole.getSubSys().equals(subSys)){
            return list;
        }
        List<AuthUserRole> child=Lists.newArrayList();
        //递归获得该角色的基础角色
        recursion(child,Lists.newArrayList(roleId),authUserRoles);
        //判空
        if(CollectionUtils.isEmpty(child)){
            return list;
        }
        //查询这些基础角色对应的菜单
        list = resourceMapper.selectResourcesByUserRoles(child, subSys).stream()
                .map(obj -> BeanUtils.copyProperties(obj, AuthResourceDetailDTO.class)
                        .setHideChildrenMenu(isHideChildrenMenu(obj.getHide()))
                )
                .collect(Collectors.toList());
        return this.getResourcesTree(list);
    }*/
    @Override
    public List<AuthResourceDetailDTO> getUserResourceByRoleId(String subSys, Integer roleId, AuthPlatformUserInfo userInfo) {
        List<AuthResourceDetailDTO> list= Lists.newArrayList();

        // 直接获取
        List<AuthUserRole> authUserRoles = this.getUserRoleDirect(userInfo,subSys,roleId);
        // 通过用户组获取
        List<AuthUsergroupRole> usergroupRoles = this.getUserRoleGroup(userInfo,subSys,roleId);
        // 求并集
        List<Integer> result = Lists.newArrayList();
        Set<Integer> set = new HashSet();
        set.addAll(authUserRoles.stream().map(a->a.getRoleId()).collect(Collectors.toList()));
        set.addAll(usergroupRoles.stream().map(a->a.getRoleId()).collect(Collectors.toList()));
        result.addAll(set);

        //查询这些基础角色对应的菜单
        list = resourceMapper.selectResourcesByUserRoles(result, subSys).stream()
                .map(obj -> BeanUtils.copyProperties(obj, AuthResourceDetailDTO.class)
                        .setHideChildrenMenu(isHideChildrenMenu(obj.getHide()))
                ).collect(Collectors.toList());
        return this.getResourcesTree(list);
    }

    /**
     * 根据roleId从用户组获取基础角色
     * @param userInfo
     * @param subSys
     * @param roleId
     * @return
     */
    private List<AuthUsergroupRole> getUserRoleGroup(AuthPlatformUserInfo userInfo, String subSys, Integer roleId) {
        List<AuthUsergroupRole> groupChild=Lists.newArrayList();
        // 查询用户所在用户组
        List<AuthUsergroup> usergroups = usergroupService.getByEnterpriseAndUserId(userInfo.getOrgId(), userInfo.getId());
        if (CollectionUtils.isEmpty(usergroups)){
            return Lists.newArrayList();
        }
        // 查询用户组对应角色
        List<AuthUsergroupRole> authUsergroupRoles = usergroupRoleMapper.selectList(new EntityWrapper<AuthUsergroupRole>()
                .eq("enterprise_id",userInfo.getOrgId())
                .eq("deleted",Status.FALSE.getKey())
                .in("usergroup_id",usergroups.stream().map(a -> a.getId()).collect(Collectors.toList())));
        if (CollectionUtils.isEmpty(authUsergroupRoles)){
            return Lists.newArrayList();
        }
        //递归获得该角色用户组内基础角色
        groupRecursion(groupChild,Lists.newArrayList(roleId),authUsergroupRoles);
        return groupChild;
    }

    /**
     * 根据roleId直接获取基础角色
     * @param userInfo
     * @param subSys
     * @param roleId
     * @return
     */
    private List<AuthUserRole> getUserRoleDirect(AuthPlatformUserInfo userInfo, String subSys, Integer roleId) {
        List<AuthResourceDetailDTO> list= Lists.newArrayList();
        //查询用户在当前企业中的角色
        List<AuthUserRole> authUserRoles = userRoleMapper.getUserRoleIdBySubSys(userInfo.getOrgId(),userInfo.getId(),subSys);
        //判空
        if(CollectionUtils.isEmpty(authUserRoles)){
            return Lists.newArrayList();
        }
        AuthRole authRole=authRoleService.selectById(roleId);
        if(authRole==null || !authRole.getSubSys().equals(subSys)){
            return Lists.newArrayList();
        }
        List<AuthUserRole> child=Lists.newArrayList();
        //递归获得该角色的基础角色
        recursion(child,Lists.newArrayList(roleId),authUserRoles);
        return child;
    }

    /**
     * 递归获得角色在用户组的基础角色
     * @param groupChild
     * @param pid
     * @param authUsergroupRoles
     */
    private List<AuthUsergroupRole> groupRecursion(List<AuthUsergroupRole> groupChild, List<Integer> pid, List<AuthUsergroupRole> authUsergroupRoles) {
        List<Integer> list=new ArrayList<>();
        for (AuthUsergroupRole item:authUsergroupRoles) {
            if(pid.contains(item.getPid())){
                if(EnumRoleType.BASE.getCode().equals(item.getRoleType())){
                    groupChild.add(item);
                }else {
                    list.add(item.getRoleId());
                }
            }
        }
        if(!CollectionUtils.isEmpty(list)){
            return groupRecursion(groupChild,list,authUsergroupRoles);
        }
        return groupChild;
    }

    /**
     * @notes: 递归获得该角色的基础角色
     * @Author: junyang.li
     * @Date: 17:16 2019/6/19
     * @param child : 容器
     * @param pid : 父角色id
     * @param authUserRoles : 目标角色
     * @return: java.util.List<com.bee.platform.user.authority.entity.AuthUserRole>
     */
    private List<AuthUserRole> recursion(List<AuthUserRole> child,List<Integer> pid,List<AuthUserRole> authUserRoles){
        List<Integer> list=new ArrayList<>();
        for (AuthUserRole item:authUserRoles) {
            if(pid.contains(item.getPid())){
                if(EnumRoleType.BASE.getCode().equals(item.getRoleType())){
                    child.add(item);
                }else {
                    list.add(item.getRoleId());
                }
            }
        }
        if(!CollectionUtils.isEmpty(list)){
            return  recursion(child,list,authUserRoles);
        }
        return child;
    }

    private boolean isHideChildrenMenu(Integer hide){
        if(hide==null){
            return false;
        }
        return hide==1;
    }

    /**
     * 根据子系统查询所有资源-后台
     * @param userInfo
     * @param subSys
     * @return
     */
    @Override
    public List<AuthResourceDetailDTO> listResourcesBySubSys(AuthPlatformUserInfo userInfo, String subSys) {
        Wrapper wrapper = new EntityWrapper<AuthResource>()
                .eq("deleted",Status.FALSE.getKey());
                //.eq("platform_admin",Status.FALSE.getKey());
        if (!StringUtils.isEmpty(subSys)){
            wrapper.and().eq("sub_sys",subSys);
        }else {
            wrapper.and().ne("sub_sys","bee_console");
        }
        List<AuthResource> resources = resourceMapper.selectList(wrapper);

        //return BeanUtils.assemble(AuthResourceDetailDTO.class,resources);
        return this.getResourcesTree(BeanUtils.assemble(AuthResourceDetailDTO.class,resources));
    }

    /**
     * @notes: 批量导入资源
     * @Author: junyang.li
     * @Date: 15:13 2019/5/28
     * @param in :
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    public ResponseResult<ResCodeEnum> excelImport(InputStream in) {
        List<AuthResource> list= ExcelUtil.getReader(in).read(0,1,AuthResource.class);
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.IMPORT_INTERFACE_FAIL);
        }
        resourceMapper.insertAll(list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     *
     * @param id
     * @return
     */
    @Override
    public ResponseResult<AuthResourceDetailDTO> getResourceDetail(String id) {
        AuthResource resource = resourceMapper.selectById(new AuthResource()
                .setId(Integer.valueOf(id)).setDeleted(Status.FALSE.getKey()));
        AuthResourceDetailDTO detailDTO = BeanUtils.copyProperties(resource,AuthResourceDetailDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,detailDTO);
    }

    /**
     *
     * @param id
     * @return
     */
    @Override
    public List<AuthResourceDetailDTO> listSubResourceBackNoPage(String id) {
        List<AuthResource> resource = resourceMapper.selectList(new EntityWrapper<AuthResource>()
                .eq("pid",id).and()
                .eq("deleted",Status.FALSE.getKey()));
        AuthResourceDetailDTO detailDTO = BeanUtils.copyProperties(resource,AuthResourceDetailDTO.class);
        return BeanUtils.assemble(AuthResourceDetailDTO.class,resource);
    }

    /**
     * 后台用户菜单
     * @param userInfo
     * @return
     */
    @Override
    public List<AuthResourceDetailDTO> resourcesByBackUser(AuthPlatformUserInfo userInfo) {
        /*List<AuthResourceDetailDTO> resourceDetailOrg = Lists.newArrayList();
        // 直接获取用户基础角色
        List<AuthFunctionRole> directList = this.getBaseRolesDirect(userInfo);
        // 根据用户组查询基础角色

        // 取并集
        List<AuthFunctionRole> baseList = CommonUtils.getUnion(directList,userGroupList);
        // 根据基础角色查询所有菜单
        if (!CollectionUtils.isEmpty(baseList)){
            resourceDetailOrg = resourceMapper.selectResourcesByOrgRoles(baseList,subSys).stream().map(obj->{
                return BeanUtils.copyProperties(obj,AuthResourceDetailDTO.class).setHideChildrenMenu(isHideChildrenMenu(obj.getHide()));
            }).collect(Collectors.toList());
        }*/
        // 查询后台用户基础角色
        List<AuthUserRoleBack> userRoleBacks = userRoleBackMapper.selectList(new EntityWrapper<>(new AuthUserRoleBack()
                .setUserId(userInfo.getId()).setLevel(4).setDeleted(Status.FALSE.getKey())));
        // 查询后台用户用户组内基础角色
        List<AuthUsergroupBack> usergroupBacks = usergroupBackService.getByEnterpriseAndUserId(null, userInfo.getId());
        List<AuthUsergroupRoleBack> usergroupRoleBacks = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(usergroupBacks)){
            usergroupRoleBacks = usergroupRoleBackService.selectList(new EntityWrapper<AuthUsergroupRoleBack>()
                    .eq("deleted", Status.FALSE.getKey())
                    .in("usergroup_id",usergroupBacks.stream().map(a -> a.getId()).collect(Collectors.toList())));
        }
        // 所有基础角色
        List<Integer> baseRoles = CommonUtils.getUnion(userRoleBacks.stream().map(a -> a.getRoleId()).collect(Collectors.toList()),
                usergroupRoleBacks.stream().map(a -> a.getRoleId()).collect(Collectors.toList()));
        if (CollectionUtils.isEmpty(userRoleBacks)){
            return Lists.newArrayList();
        }
        List<AuthResource> resources = resourceMapper.selectResourcesByUserRoles(baseRoles,"bee_console");

        return this.getResourcesTree(BeanUtils.assemble(AuthResourceDetailDTO.class,resources));
    }

    /**
     * 添加资源
     * @param userInfo
     * @param resourceRQ
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> addResource(AuthPlatformUserInfo userInfo, AuthResourceRQ resourceRQ) {
        AuthResource resource = BeanUtils.copyProperties(resourceRQ,AuthResource.class);
        resource.setDeleted(Status.FALSE.getKey())
                .setType(Status.FALSE.getKey())
                .setCreateTime(new Date());
        if (resourceMapper.insert(resource) <= ZERO){
            log.error("添加资源失败,调用{}类{}方法出错","AuthResourceServiceImpl","addResource()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * 删除资源
     * @param userInfo
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> deleteResource(AuthPlatformUserInfo userInfo, String id) {
        if (resourceMapper.updateById(new AuthResource()
                .setId(Integer.valueOf(id)).setDeleted(Status.TRUE.getKey()).setUpdateTime(new Date())) <= ZERO){
            log.error("删除资源失败,调用{}类{}方法出错","AuthResourceServiceImpl","deleteResource()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 修改资源
     * @param userInfo
     * @param resourceRQ
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> updateResource(AuthPlatformUserInfo userInfo, AuthResourceRQ resourceRQ) {
        AuthResource resource = BeanUtils.copyProperties(resourceRQ,AuthResource.class);
        resource.setUpdateTime(new Date());

        if (resourceMapper.updateById(resource) <= ZERO){
            log.error("修改资源失败,调用{}类{}方法出错","AuthResourceServiceImpl","updateResource()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


}

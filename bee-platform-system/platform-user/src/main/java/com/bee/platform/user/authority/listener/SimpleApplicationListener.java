//package com.bee.platform.user.authority.listener;
//
//import java.util.ArrayList;
//import java.util.Date;
//import java.util.HashMap;
//import java.util.List;
//import java.util.Map;
//import java.util.Objects;
//import java.util.stream.Collectors;
//
//import com.bee.platform.common.utils.BeanUtils;
//import org.apache.commons.lang3.StringUtils;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.context.ApplicationListener;
//import org.springframework.context.event.ContextRefreshedEvent;
//import org.springframework.stereotype.Component;
//import org.springframework.transaction.annotation.Transactional;
//import org.springframework.util.CollectionUtils;
//
//import com.baomidou.mybatisplus.mapper.EntityWrapper;
//import com.bee.platform.common.entity.Config;
//import com.bee.platform.common.entity.DepartmentInfo;
//import com.bee.platform.common.enums.Status;
//import com.bee.platform.common.service.ConfigService;
//import com.bee.platform.common.utils.CommonUtils;
//import com.bee.platform.common.utils.ConstantsUtil;
//import com.bee.platform.user.authority.entity.AuthEnterprise;
//import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
//import com.bee.platform.user.authority.entity.AuthFunctionRole;
//import com.bee.platform.user.authority.entity.AuthPlatformUser;
//import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
//import com.bee.platform.user.authority.entity.AuthRole;
//import com.bee.platform.user.authority.entity.AuthUserRole;
//import com.bee.platform.user.authority.service.AuthEnterpriseRoleService;
//import com.bee.platform.user.authority.service.AuthEnterpriseService;
//import com.bee.platform.user.authority.service.AuthFunctionRoleService;
//import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
//import com.bee.platform.user.authority.service.AuthPlatformUserService;
//import com.bee.platform.user.authority.service.AuthRoleService;
//import com.bee.platform.user.authority.service.AuthUserRoleService;
//import com.bee.platform.user.entity.Departments;
//import com.bee.platform.user.entity.Enterprises;
//import com.bee.platform.user.entity.EnterprisesUsers;
//import com.bee.platform.user.entity.User;
//import com.bee.platform.user.entity.UserRole;
//import com.bee.platform.user.service.DepartmentsService;
//import com.bee.platform.user.service.EnterprisesService;
//import com.bee.platform.user.service.EnterprisesUsersService;
//import com.bee.platform.user.service.UsersRolesService;
//import com.bee.platform.user.service.UsersService;
//import com.google.common.collect.Lists;
//
//import lombok.extern.slf4j.Slf4j;
//
///**
// * @description: 容器在所有的bean初始化完成后执行的方法
// * @author: junyang.li
// * @create: 2019-01-21 11:17
// **/
//@Slf4j
//@Component
//public class SimpleApplicationListener implements ApplicationListener<ContextRefreshedEvent> {
//
//    @Autowired
//    private ConfigService configService;
//
//    @Autowired
//    private AuthPlatformUserService platformUserService;
//
//    @Autowired
//    private UsersService usersService;
//
//    @Autowired
//    private EnterprisesService enterprisesService;
//
//    @Autowired
//    private EnterprisesUsersService enterprisesUsersService;
//
//    @Autowired
//    private AuthPlatformUserEnterpriseService platformUserEnterpriseService;
//
//    @Autowired
//    private UsersRolesService usersRolesService;
//
//    @Autowired
//    private AuthUserRoleService authUserRoleService;
//
//    @Autowired
//    private AuthEnterpriseService enterpriseService;
//
//    @Autowired
//    private AuthEnterpriseRoleService enterpriseRoleService;
//
//    @Autowired
//    private DepartmentsService departmentsService;
//
//    @Autowired
//    private AuthRoleService authRoleService;
//
//    @Autowired
//    private AuthFunctionRoleService fRoleService;
//
//    @Autowired
//    private AuthEnterpriseRoleService entRoleService;
//
//    /**
//     * @notes 容器在所有的bean初始化完成后执行下面的方法
//     * @Author junyang.li
//     * @Date 19:02 2019/1/22
//     **/
//    @Override
//    @Transactional(rollbackFor = Exception.class)
//    public void onApplicationEvent(ContextRefreshedEvent event) {
//        /** 是否需要容器初始化完成后执行代码块中内容的开关 **/
//        String val=configService.getConfValue(ConstantsUtil.PERMISSION_DATA_SWITCH,"1",
//                "新老权限系统用户,企业等相关数据迁移的开关，0关闭，1打开");
//        if(ConstantsUtil.TRUE.equals(val)){
//            log.info("===============================容器初始化完成，开始执行迁移代码===============================");
//            //迁移用户信息
//            createAuthUser();
//            //迁移企业
//            createEnterprise();
//            //迁移用户关联的企业信息
//            enterprisUser();
//            //迁移用户关联的角色信息
//            enterprisUserRole();
//            //迁移企业角色的关联数据
//            enterprisRole();
//            //更新Department表的Level字段
//            updateDepartmentLevel();
//            log.info("===============================迁移代码执行完成，关闭开关===============================");
//            //初始化成功，将开关关闭
//            configService.update(new Config().setConfigValue(String.valueOf(Status.FALSE.getKey())),
//                    new EntityWrapper<Config>().where("config_key={0}",ConstantsUtil.PERMISSION_DATA_SWITCH));
//        }
//    }
//
//    private void createEnterprise(){
//        List<Enterprises> enterprises=enterprisesService.selectList(new EntityWrapper<>());
//        if(!CollectionUtils.isEmpty(enterprises)){
//            List<AuthEnterprise> list=enterprises.stream().map(obj->{
//                return new AuthEnterprise().setId(obj.getId())
//                        .setSimpleName(obj.getName())
//                        .setName(obj.getName()).setType(obj.getType()).setAdmin(obj.getAdmin())
//                        .setAddress(obj.getAddress()).setContact(obj.getContact())
//                        .setDeleted(0).setLinkman(obj.getLinkman()).setStreet(obj.getStreet())
//                        .setRegionid(stringToInteger(obj.getRegionid())).setIndustry(stringToInteger(obj.getIndustry()))
//                        .setStatus(1).setCreateTime(obj.getCreateAt()).setUpdateTime(obj.getUpdateAt());
//            }).collect(Collectors.toList());
//            enterpriseService.insertAll(list);
//        }
//    }
//
//    private Integer stringToInteger(String str){
//        if(StringUtils.isBlank(str)){
//            return null;
//        }
//        try {
//            return Integer.valueOf(str);
//        }catch (NumberFormatException e){
//            return null;
//        }
//    }
//
//    /**
//     * @notes: 迁移用户数据
//     * @Author: junyang.li
//     * @Date: 10:22 2019/5/24
//     * @return: com.bee.platform.user.authority.entity.AuthPlatformUser
//     */
//    private void  createAuthUser() {
//        //查询出所有的用户
//        List<User> list=usersService.selectList(new EntityWrapper<>());
//        List<AuthPlatformUser> platformUsers=list.stream().map(user->{
//            return new AuthPlatformUser().setId(user.getId())
//                    .setPhone(user.getPhone())
//                    .setName(CommonUtils.disposePhoneNum(user.getUsername()))
//                    .setUsername(user.getUsername())
//                    .setNickname(user.getNickname())
//                    .setPassword(user.getPassword())
//                    .setHead(user.getHead())
//                    .setEmail(user.getEmail())
//                    .setQq(user.getQq())
//                    .setRegionId(user.getRegionid())
//                    .setAddress( user.getAddress())
//                    .setFixtel(user.getFixtel()).setCreateTime(user.getCreateAt()==null?new Date():user.getCreateAt())
//                    .setUpdateTime(user.getUpdateAt()==null?new Date():user.getUpdateAt())
//                    .setUpdateUser(user.getUpdateId()==null?0:user.getUpdateId())
//                    .setStatus(1)
//                    .setDeleted(0)
//                    .setActiveType(0)
//                    .setUserType(0);
//        }).collect(Collectors.toList());
//        //存入数据库中
//        platformUserService.insertAllUser(platformUsers);
//    }
//    /**
//     * @notes: 用户关联企业
//     * @Author: junyang.li
//     * @Date: 12:00 2019/5/24
//     * @return: void
//     */
//    private void  enterprisUser(){
//        //所有企业关联的用户数据
//        List<EnterprisesUsers> list=enterprisesUsersService.selectList(new EntityWrapper<>());
//
//        List<AuthPlatformUserEnterprise> userEnterprises=list.stream().map(obj->{
//            DepartmentInfo departmentInfo=departmentsService.selectByUserIdAndOrgId(obj.getUserId(),obj.getEnterpriseId());
//           return new AuthPlatformUserEnterprise()
//                   .setUserId(obj.getUserId())
//                   .setEnterpriseId(obj.getEnterpriseId())
//                   .setDepartmentsId(departmentInfo.getDepartmentId())
//                   .setPostId(departmentInfo.getPostId())
//                   .setCreateTime(new Date())
//                   .setUpdateTime(new Date())
//                   .setStatus(1)
//                   .setCreateUser(1)
//                   .setDeleted(0);
//        }).collect(Collectors.toList());
//        //删除旧数据
//        platformUserEnterpriseService.delete(new EntityWrapper<>());
//        //批量插入
//        platformUserEnterpriseService.insertAll(userEnterprises);
//    }
//    /**
//     * @notes: 用户角色对象关系数据迁移
//     * @Author: junyang.li
//     * @Date: 14:01 2019/5/24
//     * @return: void
//     */
//    private void  enterprisUserRole(){
//        List<UserRole> usersRoles=usersRolesService.selectList(new EntityWrapper<>());
//        List<AuthUserRole> list=usersRoles.stream().map(obj->{
//            int oldRoleId=obj.getRoleId();
//            if(oldRoleId==1){
//                return new AuthUserRole()
//                        .setUserId(obj.getUserId())
//                        .setEnterpriseId(obj.getOrgId())
//                        .setStatus(1)
//                        .setCreateTime(new Date())
//                        .setCreateUser(1)
//                        .setDeleted(0)
//                        .setRoleId(2)
//                        .setLevel(1)
//                        .setPid(0)
//                        .setRoleType("enterprise_admin")
//                        .setUpdateTime(new Date());
//            }
//            return null;
//        }).filter(Objects::nonNull).collect(Collectors.toList());
//        authUserRoleService.insertAll(list);
//    }
//    /**
//     * @notes: 迁移企业角色关联信息
//     * @Author: junyang.li
//     * @Date: 15:38 2019/5/24
//     * @return: void
//     */
//    private void  enterprisRole(){
//        /*List<AuthRoleDTO> dtos=Lists.newArrayList();
//        AuthRoleDTO enterpriseAdmin=new AuthRoleDTO().setId(2).setRoleName("企业管理员").setLevel(1).setRoleType("enterprise_admin")
//                .setSubSys("bee_platform");
//        dtos.add(enterpriseAdmin);
//        List<AuthEnterprise> enterprises=enterpriseService.selectList(new EntityWrapper<>());
//        List<AuthEnterpriseRole> enterpriseRoles= Lists.newArrayList();
//        enterprises.forEach(obj->dtos.forEach(item->{
//            AuthEnterpriseRole enterpriseRole=new AuthEnterpriseRole()
//                    .setEnterpriseId(obj.getId()).setRoleId(item.getId())
//                    .setRoleType(item.getRoleType()).setLevel(item.getLevel())
//                    .setDeleted(0).setStatus(1).setPid(0)
//                    .setCreateTime(new Date()).setCreateUser(1).setUpdateTime(new Date());
//            enterpriseRoles.add(enterpriseRole);
//        }));
//        enterpriseRoleService.delete(new EntityWrapper<>());
//        enterpriseRoleService.insertAll(enterpriseRoles);*/
//        List<AuthEnterpriseRole> eRoles = Lists.newArrayList();
//        List<AuthEnterprise> enterprises= enterpriseService.selectList(new EntityWrapper<>());
//        List<AuthRole> appRoles= authRoleService.selectList(new EntityWrapper<>(new AuthRole().setRoleType("application").setDeleted(0)));
//        List<Integer> appIds = appRoles.stream().map(r -> r.getId()).collect(Collectors.toList());
//        List<AuthFunctionRole> functionOneRoles= fRoleService.selectList(new EntityWrapper<AuthFunctionRole>().eq("deleted", 0).in("pid", appIds));
//        List<Integer> functionOneIds = functionOneRoles.stream().map(r -> r.getId()).collect(Collectors.toList());
//        List<AuthFunctionRole> functionTwoRoles= fRoleService.selectList(new EntityWrapper<AuthFunctionRole>().eq("deleted", 0).in("pid", functionOneIds));
//        enterprises.forEach(e -> appRoles.forEach(r -> eRoles.add(new AuthEnterpriseRole().setEnterpriseId(e.getId()).setCreateTime(new Date()).setCreateUser(1).setLevel(1).setPid(0).setRoleType("application").setRoleId(r.getId()))));
//        enterprises.forEach(e -> functionOneRoles.forEach(r -> eRoles.add(new AuthEnterpriseRole().setEnterpriseId(e.getId()).setCreateTime(new Date()).setCreateUser(1).setLevel(2).setPid(r.getPid()).setRoleType("function_one").setRoleId(r.getRoleId()))));
//        enterprises.forEach(e -> functionTwoRoles.forEach(r -> eRoles.add(new AuthEnterpriseRole().setEnterpriseId(e.getId()).setCreateTime(new Date()).setCreateUser(1).setLevel(3).setPid(r.getPid()).setRoleType("function_two").setRoleId(r.getRoleId()))));
//        enterpriseRoleService.insertAll(eRoles);
//    }
//
//    /**
//     * @notes: 修改部门的级别字段
//     * @Author: junyang.li
//     * @Date: 14:53 2019/6/3
//     * @return: void
//     */
//    public void updateDepartmentLevel(){
//        //所有部门
//       List<Departments> list= departmentsService.selectList(new EntityWrapper<>());
//       Map<Integer,Departments> map=new HashMap<>(16);
//       //遍历
//        for (int i = 0,t=list.size(); i <t; i++) {
//            Departments department=list.get(i);
//            //如果父与自己相同则为一级部门
//            if(department.getId().equals(department.getTreeId())){
//                department.setLevel(1);
//                map.put(department.getId(),department);
//            }else{
//                Departments prent=map.get(department.getTreeId());
//                if(prent==null){
//                    continue;
//                }
//                int level=prent.getLevel();
//                level++;
//                department.setLevel(level);
//                map.put(department.getId(),department);
//            }
//        }
//        //更新至数据库
//        departmentsService.updateLevelByIds(new ArrayList<>(map.values()));
//    }
//
//    /**
//     * @notes: 为所有的企业成员分配中台菜单
//     * @Author: junyang.li
//     * @Date: 2:19 2019/6/16
//     * @return: void
//     */
//    private void userAddRole(){
//       List<AuthUserRole> list= authUserRoleService.selectList(new EntityWrapper<AuthUserRole>().where("user_id=543"));
//       if(CollectionUtils.isEmpty(list)){
//           return;
//       }
//       List<UserRole> userRoles=usersRolesService.selectList(new EntityWrapper<UserRole>().where("role_id=3"));
//       if(CollectionUtils.isEmpty(userRoles)){
//           return;
//       }
//       List<AuthUserRole> now=new ArrayList<>();
//       for (UserRole item:userRoles) {
//           for (AuthUserRole obj:list) {
//               obj.setUserId(item.getUserId()).setEnterpriseId(item.getOrgId()).setId(null);
//               AuthUserRole authUserRole=authUserRoleService.selectOne(new EntityWrapper<AuthUserRole>()
//                       .where("user_id={0} and  role_id={1} and enterprise_id={2}",
//                               item.getUserId(),item.getOrgId(),obj.getRoleId()));
//               if(authUserRole==null){
//                   now.add(obj);
//               }
//           }
//       }
//       int i=1;
//        for (AuthUserRole item:now) {
//            i++;
//            log.info("插入数据:{}条",i);
//            authUserRoleService.insert(item);
//        }
//    }
//
//}
package com.bee.platform.user.authority.listener;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.entity.Config;
import com.bee.platform.common.entity.DepartmentInfo;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.utils.CommonUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
import com.bee.platform.user.authority.entity.AuthFunctionRole;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
import com.bee.platform.user.authority.entity.AuthRole;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.authority.service.AuthEnterpriseRoleService;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthFunctionRoleService;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.authority.service.AuthRoleService;
import com.bee.platform.user.authority.service.AuthUserRoleService;
import com.bee.platform.user.entity.Departments;
import com.bee.platform.user.entity.Enterprises;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.entity.User;
import com.bee.platform.user.entity.UserRole;
import com.bee.platform.user.service.DepartmentsService;
import com.bee.platform.user.service.EnterprisesService;
import com.bee.platform.user.service.EnterprisesUsersService;
import com.bee.platform.user.service.UsersRolesService;
import com.bee.platform.user.service.UsersService;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

/**
 * @description: 容器在所有的bean初始化完成后执行的方法
 * @author: junyang.li
 * @create: 2019-01-21 11:17
 **/
@Slf4j
@Component
public class SimpleApplicationListener implements ApplicationListener<ContextRefreshedEvent> {

    @Autowired
    private ConfigService configService;

    @Autowired
    private AuthPlatformUserService platformUserService;

    @Autowired
    private UsersService usersService;

    @Autowired
    private EnterprisesService enterprisesService;

    @Autowired
    private EnterprisesUsersService enterprisesUsersService;

    @Autowired
    private AuthPlatformUserEnterpriseService platformUserEnterpriseService;

    @Autowired
    private UsersRolesService usersRolesService;

    @Autowired
    private AuthUserRoleService authUserRoleService;

    @Autowired
    private AuthEnterpriseService enterpriseService;

    @Autowired
    private AuthEnterpriseRoleService enterpriseRoleService;

    @Autowired
    private DepartmentsService departmentsService;

    @Autowired
    private AuthRoleService authRoleService;

    @Autowired
    private AuthFunctionRoleService fRoleService;

    @Autowired
    private AuthEnterpriseRoleService entRoleService;
    /**
     * @notes 容器在所有的bean初始化完成后执行下面的方法
     * @Author junyang.li
     * @Date 19:02 2019/1/22
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void onApplicationEvent(ContextRefreshedEvent event) {
        /** 是否需要容器初始化完成后执行代码块中内容的开关 **/
        String val=configService.getConfValue(ConstantsUtil.PERMISSION_DATA_SWITCH,"1",
                "新老权限系统用户,企业等相关数据迁移的开关，0关闭，1打开");
        if(ConstantsUtil.TRUE.equals(val)){
            log.info("===============================容器初始化完成，开始执行迁移代码===============================");
            //迁移用户信息
            createAuthUser();
            //迁移企业
            createEnterprise();
            //迁移用户关联的企业信息
            enterprisUser();
            //迁移用户关联的角色信息
            enterprisUserRole();
            //迁移企业角色的关联数据
            enterprisRole();
            //更新Department表的Level字段
            updateDepartmentLevel();
            log.info("===============================迁移代码执行完成，关闭开关===============================");
            //初始化成功，将开关关闭
            configService.update(new Config().setConfigValue(String.valueOf(Status.FALSE.getKey())),
                    new EntityWrapper<Config>().where("config_key={0}",ConstantsUtil.PERMISSION_DATA_SWITCH));
        }
    }

    private void createEnterprise(){
        List<Enterprises> enterprises=enterprisesService.selectList(new EntityWrapper<>());
        if(!CollectionUtils.isEmpty(enterprises)){
            List<AuthEnterprise> list=enterprises.stream().map(obj->{
                return new AuthEnterprise().setId(obj.getId())
                        .setSimpleName(obj.getName())
                        .setName(obj.getName()).setType(obj.getType()).setAdmin(obj.getAdmin())
                        .setAddress(obj.getAddress()).setContact(obj.getContact())
                        .setDeleted(0).setLinkman(obj.getLinkman()).setStreet(obj.getStreet())
                        .setRegionid(stringToInteger(obj.getRegionid())).setIndustry(stringToInteger(obj.getIndustry()))
                        .setStatus(1).setCreateTime(obj.getCreateAt()).setUpdateTime(obj.getUpdateAt());
            }).collect(Collectors.toList());
            enterpriseService.insertAll(list);
        }
    }

    private Integer stringToInteger(String str){
        if(StringUtils.isBlank(str)){
            return null;
        }
        try {
            return Integer.valueOf(str);
        }catch (NumberFormatException e){
            return null;
        }
    }

    /**
     * @notes: 迁移用户数据
     * @Author: junyang.li
     * @Date: 10:22 2019/5/24
     * @return: com.bee.platform.user.authority.entity.AuthPlatformUser
     */
    private void  createAuthUser() {
        //查询出所有的用户
        List<User> list=usersService.selectList(new EntityWrapper<>());
        List<AuthPlatformUser> platformUsers=list.stream().map(user->{
            return new AuthPlatformUser().setId(user.getId())
                    .setPhone(user.getPhone())
                    .setName(CommonUtils.disposePhoneNum(user.getUsername()))
                    .setUsername(user.getUsername())
                    .setNickname(user.getNickname())
                    .setPassword(user.getPassword())
                    .setHead(user.getHead())
                    .setEmail(user.getEmail())
                    .setQq(user.getQq())
                    .setRegionId(user.getRegionid())
                    .setAddress( user.getAddress())
                    .setFixtel(user.getFixtel()).setCreateTime(user.getCreateAt()==null?new Date():user.getCreateAt())
                    .setUpdateTime(user.getUpdateAt()==null?new Date():user.getUpdateAt())
                    .setUpdateUser(user.getUpdateId()==null?0:user.getUpdateId())
                    .setStatus(1)
                    .setDeleted(0)
                    .setActiveType(0)
                    .setUserType(0);
        }).collect(Collectors.toList());
        //存入数据库中
        platformUserService.insertAllUser(platformUsers);
    }
    /**
     * @notes: 用户关联企业
     * @Author: junyang.li
     * @Date: 12:00 2019/5/24
     * @return: void
     */
    private void  enterprisUser(){
        //所有企业关联的用户数据
        List<EnterprisesUsers> list=enterprisesUsersService.selectList(new EntityWrapper<>());

        List<AuthPlatformUserEnterprise> userEnterprises=list.stream().map(obj->{
            DepartmentInfo departmentInfo=departmentsService.selectByUserIdAndOrgId(obj.getUserId(),obj.getEnterpriseId());
           return new AuthPlatformUserEnterprise()
                   .setUserId(obj.getUserId())
                   .setEnterpriseId(obj.getEnterpriseId())
                   .setDepartmentsId(departmentInfo.getDepartmentId())
                   .setPostId(departmentInfo.getPostId())
                   .setCreateTime(new Date())
                   .setUpdateTime(new Date())
                   .setStatus(1)
                   .setCreateUser(1)
                   .setDeleted(0);
        }).collect(Collectors.toList());
        //删除旧数据
        platformUserEnterpriseService.delete(new EntityWrapper<>());
        //批量插入
        platformUserEnterpriseService.insertAll(userEnterprises);
    }
    /**
     * @notes: 用户角色对象关系数据迁移
     * @Author: junyang.li
     * @Date: 14:01 2019/5/24
     * @return: void
     */
    private void  enterprisUserRole(){
        List<UserRole> usersRoles=usersRolesService.selectList(new EntityWrapper<>());
        List<AuthUserRole> list=usersRoles.stream().map(obj->{
            int oldRoleId=obj.getRoleId();
            if(oldRoleId==1){
                return new AuthUserRole()
                        .setUserId(obj.getUserId())
                        .setEnterpriseId(obj.getOrgId())
                        .setStatus(1)
                        .setCreateTime(new Date())
                        .setCreateUser(1)
                        .setDeleted(0)
                        .setRoleId(2)
                        .setLevel(1)
                        .setPid(0)
                        .setRoleType("enterprise_admin")
                        .setUpdateTime(new Date());
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
        authUserRoleService.insertAll(list);
    }
    /**
     * @notes: 迁移企业角色关联信息
     * @Author: junyang.li
     * @Date: 15:38 2019/5/24
     * @return: void
     */
    private void  enterprisRole(){
        /*List<AuthRoleDTO> dtos=Lists.newArrayList();
        AuthRoleDTO enterpriseAdmin=new AuthRoleDTO().setId(2).setRoleName("企业管理员").setLevel(1).setRoleType("enterprise_admin")
                .setSubSys("bee_platform");
        dtos.add(enterpriseAdmin);
        List<AuthEnterprise> enterprises=enterpriseService.selectList(new EntityWrapper<>());
        List<AuthEnterpriseRole> enterpriseRoles= Lists.newArrayList();
        enterprises.forEach(obj->dtos.forEach(item->{
            AuthEnterpriseRole enterpriseRole=new AuthEnterpriseRole()
                    .setEnterpriseId(obj.getId()).setRoleId(item.getId())
                    .setRoleType(item.getRoleType()).setLevel(item.getLevel())
                    .setDeleted(0).setStatus(1).setPid(0)
                    .setCreateTime(new Date()).setCreateUser(1).setUpdateTime(new Date());
            enterpriseRoles.add(enterpriseRole);
        }));
        enterpriseRoleService.delete(new EntityWrapper<>());
        enterpriseRoleService.insertAll(enterpriseRoles);*/
        List<AuthEnterpriseRole> eRoles = Lists.newArrayList();
        List<AuthEnterprise> enterprises= enterpriseService.selectList(new EntityWrapper<>());
        List<AuthRole> appRoles= authRoleService.selectList(new EntityWrapper<>(new AuthRole().setRoleType("application").setDeleted(0)).notIn("sub_sys", Lists.newArrayList("bee_supply_chain_finance","bee_trade","bee_industrial_brain")));
        List<Integer> appIds = appRoles.stream().map(r -> r.getId()).collect(Collectors.toList());
        List<AuthFunctionRole> functionOneRoles= fRoleService.selectList(new EntityWrapper<AuthFunctionRole>().eq("deleted", 0).in("pid", appIds));
        List<Integer> functionOneIds = functionOneRoles.stream().map(r -> r.getId()).collect(Collectors.toList());
        List<AuthFunctionRole> functionTwoRoles= fRoleService.selectList(new EntityWrapper<AuthFunctionRole>().eq("deleted", 0).in("pid", functionOneIds));
        enterprises.forEach(e -> appRoles.forEach(r -> eRoles.add(new AuthEnterpriseRole().setEnterpriseId(e.getId()).setCreateTime(new Date()).setUpdateTime(new Date()).setCreateUser(1).setLevel(1).setPid(0).setStatus(1).setDeleted(0).setRoleType("application").setRoleId(r.getId()))));
        enterprises.forEach(e -> functionOneRoles.forEach(r -> eRoles.add(new AuthEnterpriseRole().setEnterpriseId(e.getId()).setCreateTime(new Date()).setUpdateTime(new Date()).setCreateUser(1).setStatus(1).setLevel(2).setDeleted(0).setPid(r.getPid()).setRoleType("function_one").setRoleId(r.getRoleId()))));
        enterprises.forEach(e -> functionTwoRoles.forEach(r -> eRoles.add(new AuthEnterpriseRole().setEnterpriseId(e.getId()).setCreateTime(new Date()).setUpdateTime(new Date()).setCreateUser(1).setStatus(1).setLevel(3).setDeleted(0).setPid(r.getPid()).setRoleType("function_two").setRoleId(r.getRoleId()))));
        enterpriseRoleService.insertAll(eRoles);
    }

    /**
     * @notes: 修改部门的级别字段
     * @Author: junyang.li
     * @Date: 14:53 2019/6/3
     * @return: void
     */
    public void updateDepartmentLevel(){
        //所有部门
       List<Departments> list= departmentsService.selectList(new EntityWrapper<>());
       Map<Integer,Departments> map=new HashMap<>(16);
       //遍历
        for (int i = 0,t=list.size(); i <t; i++) {
            Departments department=list.get(i);
            //如果父与自己相同则为一级部门
            if(department.getId().equals(department.getTreeId())){
                department.setLevel(1);
                map.put(department.getId(),department);
            }else{
                Departments prent=map.get(department.getTreeId());
                if(prent==null){
                    continue;
                }
                int level=prent.getLevel();
                level++;
                department.setLevel(level);
                map.put(department.getId(),department);
            }
        }
        //更新至数据库
        departmentsService.updateLevelByIds(new ArrayList<>(map.values()));
    }
}

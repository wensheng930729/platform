package com.bee.platform.user.controller;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.RoleType;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.dao.mapper.*;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.stream.Collectors;

//@ApiIgnore
@Api(value = "SynchronizeController", tags = "SynchronizeController")
@RestController
@RequestMapping("/api_v1")
@CrossOrigin(origins = "*")
public class SynchronizeController {

    @Autowired
    private JedisService jedisService;
    @Autowired
    private UserMapper userMapper;
    @Autowired
    private EnterprisesMapper enterprisesMapper;
    @Autowired
    private EnterprisesUsersMapper enterprisesUsersMapper;
    @Autowired
    private DepartmentsMapper departmentsMapper;
    @Autowired
    private UsersDepartmentsMapper usersDepartmentsMapper;
    @Autowired
    private EnterprisesAppsMapper enterprisesAppsMapper;
    @Autowired
    private AppMapper appMapper;
    @Autowired
    private UsersService usersService;


    @Autowired
    private UserInfoUtils userInfoUtils;

    /*****************************************通达OA专有API*****************************************/
    @ApiOperation(value = "accountInfoForOa")
    @GetMapping(value = {"/oa/accountInfo"})
    public Object accountInfoForOa(HttpServletRequest request) {
        ResponseResult message;
        try {
            AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
            if(ObjectUtils.isEmpty(userInfo)){
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            Map<String,Object> result = jedisService.getObject(userInfo.getSysToken(), HashMap.class);
            Integer org_id = (Integer) result.get("orgId");
            if (ObjectUtils.isEmpty(org_id) || org_id == -1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            App app = appMapper.selectOne(new App().setName("通达OA"));
            if(ObjectUtils.isEmpty(app)){
                return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
            }
            EnterprisesApps enterpriseAppRelation = enterprisesAppsMapper.selectOne(new EnterprisesApps().setOrgId(org_id).setAppId(app.getId()));
            if (enterpriseAppRelation == null) {
                return ResponseResult.buildResponseResult(ResCodeEnum.APP_NOT_OPEN);
            }
            //是否超级管理员
            boolean isAdmin = RoleType.SUPER.getDesc().equals(userInfo.getRoleList().get(0).getRoleName());
            if (isAdmin) {
                Enterprises enterprise = enterprisesMapper.selectOne(new Enterprises().setId(org_id));
                String enterprise_name = enterprise.getName();
                Map<String, Object> json = new LinkedHashMap<>();
                json.put("org_id", enterprise.getId());
                json.put("org_name", enterprise_name);
                List<Map<String, Object>> depData = new LinkedList<>();
                //企业下部门
                List<Departments> departments = departmentsMapper.selectList(new EntityWrapper<Departments>().eq("org_id", org_id));
                for (Departments department : departments) {
                    Map<String, Object> map = new LinkedHashMap<>();
                    map.put("id", department.getId());
                    map.put("org_id", department.getOrgId());
                    map.put("parent_id", department.getTreeId());
                    map.put("name", department.getName());
                    //部门下用户
                    List<UsersDepartments> usersDepartments = usersDepartmentsMapper.selectList(
                            new EntityWrapper<UsersDepartments>().eq("department_id", department.getId()));
                    if(ObjectUtils.isEmpty(usersDepartments)){
                        return ResponseResult.buildResponseResult(ResCodeEnum.GET_DEPARTMENT_USER_RELATION_FAILED);
                    }
                    List<Integer> uIds = usersDepartments.stream().map(e -> e.getUserId()).collect(Collectors.toList());
                    List<User> users = userMapper.selectList(new EntityWrapper<User>().in("id", uIds));
                    List< Map < String, Object >> userData = new LinkedList<>();
                    for (User user : users) {
                        EnterprisesUsers relation = enterprisesUsersMapper.selectOne(new EnterprisesUsers().setId(user.getId()).setEnterpriseId(org_id));
                        if(ObjectUtils.isEmpty(relation)){
                            return ResponseResult.buildResponseResult(ResCodeEnum.GET_DEPARTMENT_USER_RELATION_FAILED);
                        }
                        String appIds = relation.getAppIds();
                        if (StringUtils.hasLength(appIds) && Arrays.asList(appIds.split(",")).contains(String.valueOf(app.getId()))) {
                            Map<String, Object> userMap = new LinkedHashMap<>();
                            userMap.put("uuid", user.getUuid());
                            userMap.put("name", relation.getNickname());
                            userMap.put("phone", user.getPhone());
                            userData.add(userMap);
                        }
                    }
                    map.put("users", userData);
                    depData.add(map);
                }
                json.put("deps", depData);
                message = ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
                message.setObject(json);
            } else {
                message = ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_ADMIN);
            }
        } catch (Exception e) {
            e.printStackTrace();
            message = ResponseResult.buildResponseResult(ResCodeEnum.BACKSTAGE_EXCEPTION);
        }
        return message;
    }

    /*****************************************odoo专有API*******************************************/
    @ApiOperation(value = "userInfoForOdoo")
    @RequestMapping(value = "/odoo/userInfo", method = RequestMethod.GET)
    public Object userInfoForOdoo(HttpServletRequest request) {
        ResponseResult message;
        try {
            AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
            if(ObjectUtils.isEmpty(userInfo)){
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            Map<String,Object> result = jedisService.getObject(userInfo.getSysToken(), HashMap.class);
            Integer org_id = (Integer) result.get("orgId");
            System.out.println("--------------------------------------------------------------------------------------------");
            System.out.println("org_id: "+org_id);
            System.out.println("--------------------------------------------------------------------------------------------");
            if (ObjectUtils.isEmpty(org_id) || org_id == -1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            //是否管理员
            boolean isAdmin = RoleType.SUPER.getDesc().equals(userInfo.getRoleList().get(0).getRoleName());
            //phone获取用户信息
            User user = userMapper.selectOne(new User().setPhone(userInfo.getPhone()));
            if(ObjectUtils.isEmpty(user)){
                return ResponseResult.buildResponseResult(ResCodeEnum.GET_USER_FAILED);
            }
            String enterprise_name = enterprisesMapper.selectOne(new Enterprises().setId(org_id)).getName();
            Map<String, Object> json = new HashMap<>();
            json.put("org_id", org_id + "");
            json.put("org_name", enterprise_name);
            json.put("user_id", user.getUuid());
            json.put("email", user.getPhone());
            json.put("name", enterprisesUsersMapper.selectOne(new EnterprisesUsers().setUserId(user.getId()).setEnterpriseId(org_id)).getNickname());
            json.put("is_admin", isAdmin + "");
            json.put("expires_in", "3600");
            message = ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            message.setObject(json);
        } catch (Exception e) {
            e.printStackTrace();
            message = ResponseResult.buildResponseResult(ResCodeEnum.BACKSTAGE_EXCEPTION);
        }
        return message;
    }

    @ApiOperation(value = "accountInfoForOdoo")
    @RequestMapping(value = "/odoo/accountInfo", method = RequestMethod.GET)
    public ResponseResult accountInfoForOdoo(HttpServletRequest request) {
        ResponseResult message;
        try {
            AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
            if(ObjectUtils.isEmpty(userInfo)){
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            Map<String,Object> result = jedisService.getObject(userInfo.getSysToken(), HashMap.class);
            Integer org_id = (Integer) result.get("orgId");
            if (ObjectUtils.isEmpty(org_id) || org_id == -1) {
                message = ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
                return message;
            }
            App app = appMapper.selectOne(new App().setName("Odoo ERP"));
            if(ObjectUtils.isEmpty(app)){
                return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
            }
            EnterprisesApps enterpriseAppRelation = enterprisesAppsMapper.selectOne(new EnterprisesApps().setOrgId(org_id).setAppId(app.getId()));
            if (ObjectUtils.isEmpty(enterpriseAppRelation)) {
                message = ResponseResult.buildResponseResult(ResCodeEnum.APP_NOT_OPEN);
                return message;
            }
            //是否超级管理员
            boolean isAdmin = RoleType.SUPER.getDesc().equals(userInfo.getRoleList().get(0).getRoleName());
            if (isAdmin) {
                Map<String, Object> json = new HashMap<>();
                Enterprises enterprise = enterprisesMapper.selectById(org_id);
                if(ObjectUtils.isEmpty(enterprise)){
                    return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
                }
                String enterprise_name = enterprise.getName();
                //phone获取用户信息
                User user = userMapper.selectOne(new User().setPhone(userInfo.getPhone()));
                if(ObjectUtils.isEmpty(user)){
                    return ResponseResult.buildResponseResult(ResCodeEnum.GET_USER_FAILED);
                }
                json.put("org_id", org_id + "");
                json.put("org_name", enterprise_name);
                json.put("expires_in", "3600");
                Map<String, Object> admin = new HashMap<>();
                admin.put("user_id", user.getUuid());
                admin.put("email", user.getPhone());
                admin.put("name", enterprisesUsersMapper.selectOne(new EnterprisesUsers().setUserId(user.getId()).setEnterpriseId(org_id)).getNickname());
                admin.put("is_admin", "true");
                json.put("admin", admin);
                //企业下用户
                List<EnterprisesUsers> enterprisesUsers = enterprisesUsersMapper.selectList(
                        new EntityWrapper<EnterprisesUsers>().eq("enterprise_id", enterprise.getId()));
                if(ObjectUtils.isEmpty(enterprisesUsers)){
                    return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
                }
                List<Integer> uIds = enterprisesUsers.stream().map(e -> e.getUserId()).collect(Collectors.toList());
                List<User> users = userMapper.selectList(new EntityWrapper<User>().in("id", uIds));
                List<Object> staffs = new ArrayList<>();
                for (User user1 : users) {
                    EnterprisesUsers relation = enterprisesUsersMapper.selectOne(new EnterprisesUsers().setUserId(user1.getId()).setEnterpriseId(enterprise.getId()));
                    String appIds = relation.getAppIds();
                    if (StringUtils.hasLength(appIds) && Arrays.asList(appIds.split(",")).contains(String.valueOf(app.getId()))) {
                        Map<String, Object> staffMap = new HashMap<>();
                        if (!user1.getPhone().equals(user.getPhone())) {
                            staffMap.put("user_id", user1.getUuid());
                            staffMap.put("email", user1.getPhone());
                            staffMap.put("name", enterprisesUsersMapper.selectOne(new EnterprisesUsers().setUserId(user1.getId()).setEnterpriseId(enterprise.getId())).getNickname());
                            staffMap.put("is_admin", false);
                            staffs.add(staffMap);
                        }
                    }
                }
                json.put("staffs", staffs);
                message = ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
                message.setObject(json);
            } else {
                message = ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_ADMIN);
            }
        } catch (Exception e) {
            e.printStackTrace();
            message = ResponseResult.buildResponseResult(ResCodeEnum.BACKSTAGE_EXCEPTION);
        }
        return message;
    }

    /*****************************************物流专有API*******************************************/
    @ApiOperation(value = "userInfoForLogistics")
    @GetMapping("/logistics/userInfo")
    public ResponseResult userInfoForLogistics(HttpServletRequest request) {
        ResponseResult message;
        try {
            AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
            if(ObjectUtils.isEmpty(userInfo)){
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            Map<String,Object> result = jedisService.getObject(userInfo.getSysToken(), HashMap.class);
            Integer org_id = (Integer) result.get("orgId");
            if (ObjectUtils.isEmpty(org_id) || org_id == -1) {
                message = ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
                return message;
            }
            String enterprise_name = enterprisesMapper.selectOne(new Enterprises().setId(org_id)).getName();
            //phone获取用户信息
            User user = userMapper.selectOne(new User().setPhone(userInfo.getPhone()));
            if(ObjectUtils.isEmpty(user)){
                return ResponseResult.buildResponseResult(ResCodeEnum.GET_USER_FAILED);
            }
            Map<String, Object> json = new HashMap<>();
            json.put("org_name", enterprise_name);
            json.put("uuid", user.getUuid());
            json.put("phone", user.getPhone());
            json.put("photo", user.getHead());
            json.put("nickname", enterprisesUsersMapper.selectOne(new EnterprisesUsers().setUserId(user.getId()).setEnterpriseId(org_id)).getNickname());
            json.put("type", 1);
            message = ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            message.setObject(json);
        } catch (Exception e) {
            e.printStackTrace();
            message = ResponseResult.buildResponseResult(ResCodeEnum.BACKSTAGE_EXCEPTION);
        }
        return message;
    }

    @ApiOperation(value = "accountInfoForLogistics")
    @GetMapping("/logistics/accountInfo")
    public ResponseResult<List<Map<String, Object>>> accountInfoForLogistics(HttpServletRequest request) {
        ResponseResult message;
        try {
            AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
            if(ObjectUtils.isEmpty(userInfo)){
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            List<Map<String, Object>> data = new ArrayList<>();
            Map<String,Object> result = jedisService.getObject(userInfo.getSysToken(), HashMap.class);
            Integer org_id = (Integer) result.get("orgId");
            if (ObjectUtils.isEmpty(org_id) || org_id == -1) {
                return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
            }
            Enterprises enterprise = enterprisesMapper.selectOne(new Enterprises().setId(org_id));
            String enterprise_name = enterprise.getName();
            //企业下用户
            List<EnterprisesUsers> enterprisesUsers = enterprisesUsersMapper.selectList(
                    new EntityWrapper<EnterprisesUsers>().eq("enterprise_id", enterprise.getId()));
            if(ObjectUtils.isEmpty(enterprisesUsers)){
                return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
            }
            List<Integer> uIds = enterprisesUsers.stream().map(e -> e.getUserId()).collect(Collectors.toList());
            List<User> users = userMapper.selectList(new EntityWrapper<User>().in("id", uIds));
            if(CollectionUtils.isEmpty(users)){
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
            }
            Map<String, Object> json;
            for (User user : users) {
                json = new HashMap<>();
                json.put("org_name", enterprise_name);
                json.put("uuid", user.getUuid());
                json.put("phone", user.getPhone());
                json.put("photo", user.getHead());
                json.put("nickname", enterprisesUsersMapper.selectOne(new EnterprisesUsers().setUserId(user.getId()).setEnterpriseId(org_id)).getNickname());
                json.put("type", 1);
                data.add(json);
            }
            message = ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            message.setObject(data);
        } catch (Exception e) {
            e.printStackTrace();
            message = ResponseResult.buildResponseResult(ResCodeEnum.BACKSTAGE_EXCEPTION);
        }
        return message;
    }
}

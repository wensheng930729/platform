package com.bee.platform.user.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.dao.mapper.MPlatformManagersMapper;
import com.bee.platform.user.dao.mapper.MRolesMapper;
import com.bee.platform.user.entity.MRoles;
import com.bee.platform.user.entity.ManagersRoles;
import com.bee.platform.user.entity.PlatformManagers;
import com.bee.platform.user.service.ManagersRolesService;
import com.bee.platform.user.service.PlatformManagersService;
import com.bee.platform.user.service.UsersService;

import lombok.extern.slf4j.Slf4j;


/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author qhwang123
 * @since 2019-03-14
 */
@Service
@Slf4j
public class PlatformManagersServiceImpl extends ServiceImpl<MPlatformManagersMapper, PlatformManagers> implements PlatformManagersService {

    @Autowired
    private MPlatformManagersMapper platformManagersMapper;

    @Autowired
    private ManagersRolesService managersRolesService;
    @Autowired
    private MRolesMapper mRolesMapper;

    @Autowired
    private UsersService usersService;




    /**
     * @notes: 通过用户id获得用户名称
     * @Author: junyang.li
     * @Date: 17:33 2019/4/30
     * @params: [ids] 用户角色id
     * @return: java.util.Map<java.lang.Integer,java.lang.String>
     **/
    @Override
    public Map<Integer, String> getManagerNameById(Set<Integer> ids) {
        if(CollectionUtils.isEmpty(ids)){
            return new HashMap<>(1);
        }
        Map<Integer, String> map=new HashMap<>(16);
        List<PlatformManagers> managers=platformManagersMapper.getManagerNameById(ids);
        managers.forEach(obj->map.put(obj.getManagerId(),obj.getNickname()));
        return map;
    }


    /**
     * 通过用户名查询管理员信息
     * @param userName
     * @return
     */
    @Override
    public PlatformManagers getManagerByName(String userName) {
        PlatformManagers platformManagers = platformManagersMapper.selectOne(new PlatformManagers().setUsername(userName));
        return platformManagers;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updatePassword(HttpServletRequest request, String old_password, String password) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        try {
            PlatformManagers manager = getManagerByName(userInfo.getUsername());
            if (manager.getPassword().equals(old_password)) {
                manager.setPassword(password);
                manager.setUpdateAt(new Date());
                if (platformManagersMapper.updateById(manager) <= 0) {
                    log.error("调用{}的{}方法出错，manager保存失败","updatePassword", "updateById()");
                    return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            } else {
                return ResponseResult.buildResponseResult(ResCodeEnum.OLD_PASSWORD_FAIL);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updatePassword(int id, String password) {
        try {
            PlatformManagers manager = platformManagersMapper.selectOne(new PlatformManagers().setManagerId(id));
            if (!manager.getPassword().equals(password)) {
                manager.setPassword(password);
                manager.setUpdateAt(new Date());
                if (platformManagersMapper.updateById(manager) <= 0) {
                    log.error("调用{}的{}方法出错，manager保存失败","updatePassword", "updateById()");
                    return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
    }

    @Override
    public ResponseResult getAdmins() {
        ResponseResult result;
        try {
            List<PlatformManagers> managers = platformManagersMapper.selectList(new EntityWrapper<>());
            List<HashMap<String, Object>> list = new ArrayList<>();
            for (PlatformManagers manager : managers) {
                HashMap<String, Object> map = new HashMap<>(20);
                map.put("id", manager.getManagerId());
                map.put("nickname", manager.getNickname());
                map.put("phone", manager.getPhone());
                map.put("password", manager.getPassword());
                //查询管理员角色管理表
                List<ManagersRoles> managersRoles = managersRolesService.selectList(
                        new EntityWrapper<ManagersRoles>().eq("managerId", manager.getManagerId()));
                String role = "";
                if (!CollectionUtils.isEmpty(managersRoles)) {
                    for (ManagersRoles managersRole : managersRoles) {
                        //查询角色信息
                        MRoles mRole = mRolesMapper.selectById(managersRole.getRoleId());
                        if (StringUtils.isEmpty(role)) {
                            role += mRole.getRoleName();
                        } else {
                            role += ("," + mRole.getRoleName());
                        }
                    }
                }
                map.put("role", role);
                list.add(map);
            }
            result = ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
        } catch (Exception e) {
            e.printStackTrace();
            result = ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addAdmin(String nickname, String username, String password) {
        try {
            //查询管理员是否存在
            if (platformManagersMapper.selectCount(new EntityWrapper<PlatformManagers>().eq("username", username)) == 0) {
                PlatformManagers manager = new PlatformManagers();
                manager.setNickname(nickname);
                manager.setUsername(username);
                manager.setPhone(username);
                manager.setPassword(password);
                manager.setCreateAt(new Date());
                manager.setUpdateAt(new Date());
                if (platformManagersMapper.insert(manager) == 0) {
                    log.error("调用{}的{}方法出错， manager保存失败","addAdmin", "insert()");
                    return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
                }
                ManagersRoles managersRoles = new ManagersRoles();
                managersRoles.setManagerId(manager.getManagerId());
                //查询管理员角色
                MRoles mRole = mRolesMapper.selectOne(new MRoles().setRoleName("manager"));
                managersRoles.setRoleId(mRole.getRoleId());
                //保存管理员角色信息
                if (!managersRolesService.insert(managersRoles)) {
                    log.error("调用{}的{}方法出错， managersRoles保存失败","addAdmin", "insert()");
                    return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            } else {
                //管理员已经存在
                return ResponseResult.buildResponseResult(ResCodeEnum.USER_ALREADY_ADMIN);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteManager(HttpServletRequest request, int id) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        try {
            //查询管理员是否存在
            PlatformManagers platformManagers = platformManagersMapper.selectById(id);
            if (ObjectUtils.isEmpty(platformManagers)) {
                //用户不存在
                return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
            } else {
                //判断删除的是不是管理员自己
                PlatformManagers manager = platformManagersMapper.selectOne(new PlatformManagers().setUsername(userInfo.getUsername()));
                if (platformManagers.getManagerId().equals(manager.getManagerId())) {
                    //不能删除自己
                    return ResponseResult.buildResponseResult(ResCodeEnum.ADMIN_CANNOT_DELETE_SELF);
                }
                //删除
                if (platformManagersMapper.deleteById(id) == 0) {
                    log.error("删除失败");
                    return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

    @Override
    public ResponseResult getName(HttpServletRequest request) {
        UserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        try {
            //根据登录名查询管理员信息
            PlatformManagers manager = platformManagersMapper.selectOne(new PlatformManagers().setUsername(userInfo.getUsername()));
            if (ObjectUtils.isEmpty(manager)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
            } else {
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, manager.getNickname());
            }
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

}

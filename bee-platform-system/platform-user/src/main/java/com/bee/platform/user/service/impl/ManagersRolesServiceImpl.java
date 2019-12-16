package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.MRoleInfo;
import com.bee.platform.user.dao.mapper.MManagersRolesMapper;
import com.bee.platform.user.entity.ManagersRoles;
import com.bee.platform.user.service.MRolesService;
import com.bee.platform.user.service.ManagersRolesService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @notes 管理后台成员角色关联对象
 * @Author junyang.li
 * @Date 15:01 2019/4/29
 **/
@Slf4j
@Service
public class ManagersRolesServiceImpl extends ServiceImpl<MManagersRolesMapper, ManagersRoles> implements ManagersRolesService {

    @Autowired
    private MRolesService rolesService;

    @Autowired
    private MManagersRolesMapper managersRolesMapper;
    /**
     * @notes 通过用户id获得用户的角色
     * @Author junyang.li
     * @Date 15:01 2019/4/29
     **/
    @Override
    public MRoleInfo getUserRoles(Integer managerId) {
        //查询该用户对应的角色
        ManagersRoles managersRoles=managersRolesMapper.selectOne(new ManagersRoles().setManagerId(managerId));
        if(managersRoles==null){
            return null;
        }
        return rolesService.getRoleByRoleId(managersRoles.getRoleId());
    }

    /**
     * @notes: 通过用户id查询用户角色，返回map对象
     * @Author: junyang.li
     * @Date: 11:21 2019/5/9
     * @param managerIds : 用户角色
     * @return: java.util.Map<java.lang.Integer,com.bee.platform.common.entity.MRoleInfo>
     */
    @Override
    public Map<Integer, MRoleInfo> getRoleByManagerIds(List<Integer> managerIds) {
        Map<Integer, MRoleInfo> map=new HashMap<>(16);
        if(CollectionUtils.isEmpty(managerIds)){
            return map;
        }
        //查询用户角色关联关系
        List<ManagersRoles> managersRoles=managersRolesMapper.selectList(new EntityWrapper<ManagersRoles>()
                .in("manager_id",managerIds));
        if(CollectionUtils.isEmpty(managersRoles)){
            return map;
        }
        //获得角色集
        Set<Integer> roleIds=managersRoles.stream().map(ManagersRoles::getRoleId).collect(Collectors.toSet());
        Map<Integer,MRoleInfo> roleMap=rolesService.searchRolesByRoleIds(new ArrayList<>(roleIds));
        //遍历对象
        for (ManagersRoles item:managersRoles) {
            MRoleInfo info=roleMap.get(item.getRoleId());
            map.put(item.getManagerId(),info);
        }
        return map;
    }

}

package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.dao.mapper.URolesMapper;
import com.bee.platform.user.entity.Role;
import com.bee.platform.user.service.URolesService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-03-04
 */
@Slf4j
@Service
public class URolesServiceImpl extends ServiceImpl<URolesMapper, Role> implements URolesService {


    @Autowired
    private JedisService jedisService;

    @Autowired
    private URolesMapper uRolesMapper;
    /**
     * @notes 获得所有的角色
     * @Author junyang.li
     * @Date 17:05 2019/3/5
     **/
    @Override
    public List<Role> getAllRole() {
        try {
            String str = jedisService.get(ConstantsUtil.SYSTEM_ALL_ROLE);
            if (StringUtils.isEmpty(str)) {
                List<Role> roles = allRoles();
                //存入缓存中
                jedisService.set(ConstantsUtil.SYSTEM_ALL_ROLE, JSONObject.toJSONString(roles), ConstantsUtil.OVERDUE);
                return roles;
            }
            return JSONArray.parseArray(str, Role.class);
        } catch (Exception e) {
            log.error("获取所有角色信息异常，异常信息是:{}", e);
            return allRoles();
        }
    }

    /**
     * @notes 查询角色
     * @Author junyang.li
     * @Date 17:09 2019/3/5
     **/
    @Override
    public Role selectOne(Integer roleId) {
        List<Role> list=getAllRole();
        for (Role item:list) {
            if(item.getId().equals(roleId)){
                return item;
            }
        }
        log.error("传入的角色id是：{}，未查询到对应角色信息{}，信息有误。",roleId, JSON.toJSONString(list));
        return null;
    }


    public List<Role> allRoles(){
        return uRolesMapper.selectList(new EntityWrapper<Role>());
    }

}

package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.MRoleInfo;
import com.bee.platform.common.enums.MResourceType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.dao.mapper.MResourceMapper;
import com.bee.platform.user.dao.mapper.MRoleResourceMapper;
import com.bee.platform.user.dto.MResourceDTO;
import com.bee.platform.user.entity.MResource;
import com.bee.platform.user.service.MResourceService;
import com.bee.platform.user.service.MRoleRoleService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import redis.clients.jedis.exceptions.JedisException;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-13
 */
@Slf4j
@Service
public class MResourceServiceImpl extends ServiceImpl<MResourceMapper, MResource> implements MResourceService {

    @Autowired
    private  MResourceMapper resourceMapper;

    @Autowired
    private MRoleRoleService roleRoleService;

    @Autowired
    private MRoleResourceMapper roleResourceMapper;

    @Autowired
    private JedisService jedisService;

    /**
     * @notes: 获得所有的资源
     * @Author: junyang.li
     * @Date: 10:33 2019/5/14
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    @Override
    public List<MResourceDTO> getAllResource() {
        try {
            String val=jedisService.get(ConstantsUtil.MANAGER_ALL_RESOURCE);
            if(val==null){
                List<MResource> list=selectAllResource();
                List<MResourceDTO> dtos=list.stream().map(obj->{
                    return BeanUtils.copyProperties(obj,MResourceDTO.class)
                            .setHideChildrenInMenu(Status.checkStatus(obj.getHideChildrenInMenu()));
                }).collect(Collectors.toList());
                jedisService.set(ConstantsUtil.MANAGER_ALL_RESOURCE,JSONObject.toJSONString(dtos),ConstantsUtil.OVERDUE);
                return dtos;
            }
            return JSONObject.parseArray(val,MResourceDTO.class);
        }catch (JedisException e){
            log.error("获得所有资源时连接缓存异常，异常信息是：{}",e);
            List<MResource> list=selectAllResource();
            return BeanUtils.assemble(MResourceDTO.class,list);
        }
    }

    /**
     * @notes: 菜单树
     * @Author: junyang.li
     * @Date: 13:34 2019/5/13
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    @Override
    public List<MResourceDTO> menuTree() {
        List<MResourceDTO> list=getResourceByType(MResourceType.MENU);
        Map<Integer,MResourceDTO> map=new HashMap<>(16);
        list.forEach(obj->map.put(obj.getResourceId(),obj));
        List<MResourceDTO> menus=new ArrayList<>();
        //parent_id为0 则表示一级菜单
        int zero= Status.FALSE.getKey();
        //只遍历一次，把所有的子菜单往对应的父菜单中加
        list.stream().forEach(obj->{
            if(zero ==obj.getParentId()){
                menus.add(obj);
            }else {
                MResourceDTO parent = map.get(obj.getParentId());
                parent.isNull().add(obj);
            }
        });
        return menus;
    }

    /**
     * @notes: 查询当前用户可访问的的菜单树
     * @Author: junyang.li
     * @Date: 9:15 2019/5/14
     * @param roleInfo : 当前用户的角色
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    @Override
    public List<MResourceDTO> menuSelfTree(MRoleInfo roleInfo) {
        //角色信息
        try {
           String value= jedisService.get(ConstantsUtil.ROLE_MENU_TREE+roleInfo.getRoleId());
           if(value==null){
               return menuTree(roleInfo);
           }
           return JSONObject.parseArray(value,MResourceDTO.class);
        }catch (JedisException e){
            log.error("从缓存中查询角色的菜单树失败，异常信息是：{}",e);
            //遍历成树结构
            return menuTree(roleInfo);
        }
    }

    /**
     * @notes: 根据角色查询角色对应的菜单树
     * @Author: junyang.li
     * @Date: 11:27 2019/5/14
     * @param roleInfo : 当前角色
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    private List<MResourceDTO> menuTree(MRoleInfo roleInfo) {
        //查询角色组关联的基础角色
        List<Integer> roleIds=roleRoleService.getChildIdByParentId(roleInfo.getRoleId());
        //查询角色关联的菜单
        List<MResourceDTO> list=this.getMenuByRoleIds(roleIds);
        if(CollectionUtils.isEmpty(list)){
            return new ArrayList<>();
        }
        Map<Integer,MResourceDTO> map=new HashMap<>(16);
        list.forEach(obj->map.put(obj.getResourceId(),obj));
        List<MResourceDTO> menus=new ArrayList<>();
        Map<String,Integer> button=new HashMap<>(16);
        //parent_id为0 则表示一级菜单
        int zero= Status.FALSE.getKey();
        List<String> buttonPath=getButtonPathByRedis();
        //只遍历一次，把所有的子菜单往对应的父菜单中加
        list.stream().forEach(obj->{
            if(zero ==obj.getParentId()){
                menus.add(obj);
            }else if(MResourceType.BUTTON.getKey()==obj.getResourceType()){
                MResourceDTO parent=map.get(obj.getParentId());
                button.put(parent.getPath(),Status.TRUE.getKey());
            }else {
                MResourceDTO parent = map.get(obj.getParentId());
                parent.isNull().add(obj);
            }
        });
        //迭代
        buttonPath.forEach(obj->{
            Integer value=button.get(obj);
            if(value==null){
                button.put(obj,zero);
            }
        });
        //存入缓存中
        jedisService.set(ConstantsUtil.ROLE_MENU_TREE+roleInfo.getRoleId(),
                JSONObject.toJSONString(menus),ConstantsUtil.OVERDUE);
        jedisService.set(ConstantsUtil.ROLE_BUTTON+roleInfo.getRoleId(),
                JSONObject.toJSONString(button),ConstantsUtil.OVERDUE);
        return menus;
    }

    /**
     * @notes: 从缓存中获取按钮对应的页面的path，path作为键返回给前端
     * @Author: junyang.li
     * @Date: 11:03 2019/5/14
     * @return: java.util.List<java.lang.String>
     */
    @Override
    public List<String> getButtonPathByRedis(){
        try {
            String val=jedisService.get(ConstantsUtil.MANAGER_ALL_BUTTON);
            if(val==null){
                List<String> list= buttonPath();
                jedisService.set(ConstantsUtil.MANAGER_ALL_BUTTON,
                        JSONObject.toJSONString(list),ConstantsUtil.OVERDUE);
                return list;
            }
            return JSONObject.parseArray(val,String.class);
        }catch (JedisException e){
            log.error("从缓存中获取按钮对应的页面path异常，异常信息是:{}",e);
            return buttonPath();
        }
    }


    private static final String[] BUTTON=new String[]{"/openApp","/user/details","/companyManage/detail",
            "/setting","/workAndSug/workDetails","/consultation","/crm","/sysHelp","/permission"};

    /**
     * @notes: 查询当前用户的按钮，键为前端页面标识path，唯一。值 0 false ，1 true
     * @Author: junyang.li
     * @Date: 11:29 2019/5/14
     * @return: java.util.Map<java.lang.String,java.lang.Integer>
     */
    @Override
    public Map<String, Object> getSelfButton() {
        List<String> button= Arrays.asList(BUTTON);
        Map<String, Object> map=new HashMap<>(16);
        button.forEach(obj->map.put(obj,1));
        return map;
    }

    /**
     * @notes: 查询按钮对应的页面的path，并放入缓存中，path作为键返回给前端
     * @Author: junyang.li
     * @Date: 11:03 2019/5/14
     * @return: java.util.List<java.lang.String>
     */
    private List<String> buttonPath(){
        //获得所有菜单的父id
       List<Integer> parentIds=resourceMapper.getParentIdByType(MResourceType.BUTTON.getKey());
       if(CollectionUtils.isEmpty(parentIds)){
           return new ArrayList<>();
       }
        List<MResourceDTO> list=getAllResource();
       //根据父id查询父id的path
        return list.stream().filter(obj->parentIds.contains(obj.getResourceId()))
                .map(MResourceDTO::getPath).collect(Collectors.toList());
    }

    /**
     * @notes: 根据角色列表查询对应的菜单详细
     * @Author: junyang.li
     * @Date: 9:33 2019/5/14
     * @param roleIds : 角色集合
     * @return: java.util.List<com.bee.platform.user.entity.MResource>
     */
    @Override
    public List<MResourceDTO> getMenuByRoleIds(List<Integer> roleIds) {
        if(CollectionUtils.isEmpty(roleIds)){
            //不能访问任何菜单
            return new ArrayList<>();
        }
        List<Integer> resourceIds=roleResourceMapper.getResourceIdByRoleIds(roleIds);
        //根据资源id查询资源详细
        return getResourceByIds(resourceIds);
    }

    /**
     * @notes: 根据类型查询资源
     * @Author: junyang.li
     * @Date: 13:39 2019/5/13
     * @param resourceType : 资源类型
     * @return: java.util.List<com.bee.platform.user.entity.MResource>
     */
    private List<MResourceDTO> getResourceByType(MResourceType resourceType){
        List<MResourceDTO> list=getAllResource();
        int type= resourceType.getKey();
        return list.stream().map(obj->{
            if(type==obj.getResourceType()){
               return obj;
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
    }

    /**
     * @notes: 根据资源id查询资源
     * @Author: junyang.li
     * @Date: 9:51 2019/5/14
     * @param resourceIds : 资源id
     * @return: java.util.List<com.bee.platform.user.entity.MResource>
     */
    private List<MResourceDTO> getResourceByIds(List<Integer> resourceIds){
        if(CollectionUtils.isEmpty(resourceIds)){
            //没有资源可以访问
            return new ArrayList<>();
        }
        List<MResourceDTO> list=getAllResource();
        return list.stream().map(obj->{
            if(resourceIds.contains(obj.getResourceId())){
                return obj;
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
    }

    /**
     * @notes: 从数据库中查询所有的资源
     * @Author: junyang.li
     * @Date: 10:50 2019/5/14
     * @return: java.util.List<com.bee.platform.user.entity.MResource>
     */
    private List<MResource> selectAllResource(){
        return resourceMapper.selectList(new EntityWrapper<MResource>().where("status =1"));
    }
}

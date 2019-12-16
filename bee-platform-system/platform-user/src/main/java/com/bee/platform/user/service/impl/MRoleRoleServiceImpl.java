package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.enums.ManagerRoleType;
import com.bee.platform.user.dao.mapper.MRoleRoleMapper;
import com.bee.platform.user.entity.MRoleRole;
import com.bee.platform.user.service.MRoleRoleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @notes 角色与角色之间的关联表 服务实现类
 * @Author junyang.li
 * @Date 15:22 2019/4/30
 **/
@Service
public class MRoleRoleServiceImpl extends ServiceImpl<MRoleRoleMapper, MRoleRole> implements MRoleRoleService {

    @Autowired
    private MRoleRoleMapper roleRoleMapper;

    /**
     * @notes 通过子角色id查询上级菜单id
     * @Author junyang.li
     * @Date 14:42 2019/4/30
     **/
    @Override
    public Set<Integer> getParentIdByChildId( ManagerRoleType type,List<Integer> childIds) {
        return roleRoleMapper.getParentIdByChildId(type.getKey(),childIds);
    }

    /**
     * @notes  插入
     * @Author junyang.li
     * @Date 14:52 2019/4/30
     **/
    @Override
    public void insertAll(List<MRoleRole> list) {
        roleRoleMapper.insertAll(list);
    }

    /**
     * @notes: 通过父角色id获得子角色id
     * @Author: junyang.li
     * @Date: 16:12 2019/5/8
     * @param parentId :  父id
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> getChildIdByParentId(Integer parentId) {
        if(parentId==null){
            return null;
        }
        List<MRoleRole> selectRoles=roleRoleMapper.selectList(new EntityWrapper<MRoleRole>().where("parent_role_id={0}",parentId));
        if(!CollectionUtils.isEmpty(selectRoles)){
            return  selectRoles.stream().map(MRoleRole::getChildRoleId).collect(Collectors.toList());
        }
        return null;
    }

    /**
     * @notes: 通过父角色id获得子角色id
     * @Author: junyang.li
     * @Date: 16:12 2019/5/8
     * @param parentIds :  父ids
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> getChildIdByParentIds(List<Integer> parentIds) {
        if(CollectionUtils.isEmpty(parentIds)){
            return null;
        }
        List<MRoleRole> selectRoles=roleRoleMapper.selectList(new EntityWrapper<MRoleRole>().in("parent_role_id",parentIds));
        if(!CollectionUtils.isEmpty(selectRoles)){
            return  selectRoles.stream().map(MRoleRole::getChildRoleId).collect(Collectors.toList());
        }
        return null;
    }
}

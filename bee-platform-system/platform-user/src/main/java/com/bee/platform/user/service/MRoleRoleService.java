package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.enums.ManagerRoleType;
import com.bee.platform.user.entity.MRoleRole;

import java.util.List;
import java.util.Set;


/**
 * @notes 角色与角色之间的关联表 服务实现类
 * @Author junyang.li
 * @Date 15:22 2019/4/30
 **/
public interface MRoleRoleService extends IService<MRoleRole> {

    /**
     * @notes 通过子角色id查询上级菜单id
     * @Author junyang.li
     * @Date 14:42 2019/4/30
     **/
    Set<Integer> getParentIdByChildId(ManagerRoleType type,List<Integer> childIds);
    /**
     * @notes  插入
     * @Author junyang.li
     * @Date 14:52 2019/4/30
     **/
    void insertAll(List<MRoleRole> list);
    /**
     * @notes: 通过父角色id获得子角色id
     * @Author: junyang.li
     * @Date: 16:12 2019/5/8
     * @param parentId :  父id
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> getChildIdByParentId(Integer parentId);
    /**
     * @notes: 通过父角色id获得子角色id
     * @Author: junyang.li
     * @Date: 16:12 2019/5/8
     * @param parentIds :  父ids
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> getChildIdByParentIds(List<Integer> parentIds);
}

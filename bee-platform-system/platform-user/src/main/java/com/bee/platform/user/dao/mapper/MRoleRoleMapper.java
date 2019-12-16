package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.MRoleRole;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * 角色与角色之间的关联表 Mapper 接口
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-28
 */
public interface MRoleRoleMapper extends BaseMapper<MRoleRole> {
    /**
     * @notes 通过子角色id查询上级菜单id
     * @Author junyang.li
     * @Date 14:42 2019/4/30
     **/
    Set<Integer> getParentIdByChildId(int roleType,List<Integer> list);
    /**
     * @notes  插入
     * @Author junyang.li
     * @Date 14:52 2019/4/30
     **/
    void insertAll(List<MRoleRole> list);
    /**
     * 通过子角色查询mangagerIds
     * @param ChildId
     * @return
     */
    List<Integer> getManagerIdsByChildId(Integer ChildId);
    
}

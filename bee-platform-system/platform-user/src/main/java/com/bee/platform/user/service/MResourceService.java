package com.bee.platform.user.service;

import com.bee.platform.common.entity.MRoleInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.user.dto.MResourceDTO;
import com.bee.platform.user.entity.MResource;
import com.baomidou.mybatisplus.service.IService;
import io.swagger.models.auth.In;

import java.util.List;
import java.util.Map;

/**
 * 资源相关
 * @author cheng.ke123
 * @since 2019-05-13
 */
public interface MResourceService extends IService<MResource> {

    /**
     * @notes: 获得所有的资源
     * @Author: junyang.li
     * @Date: 10:33 2019/5/14
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    List<MResourceDTO> getAllResource();
    /**
     * @notes: 菜单树
     * @Author: junyang.li
     * @Date: 13:34 2019/5/13
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    List<MResourceDTO> menuTree();
    /**
     * @notes: 查询当前用户可访问的的菜单树
     * @Author: junyang.li
     * @Date: 9:15 2019/5/14
     * @param roleInfo : 当前用户的角色
     * @return: java.util.List<com.bee.platform.user.dto.MResourceDTO>
     */
    List<MResourceDTO> menuSelfTree(MRoleInfo roleInfo);
    /**
     * @notes: 根据角色列表查询对应的菜单详细
     * @Author: junyang.li
     * @Date: 9:33 2019/5/14
     * @param roleIds : 角色集合
     * @return: java.util.List<com.bee.platform.user.entity.MResource>
     */
    List<MResourceDTO> getMenuByRoleIds(List<Integer> roleIds);

    /**
     * @notes: 从缓存中获取按钮对应的页面的path，path作为键返回给前端
     * @Author: junyang.li
     * @Date: 11:03 2019/5/14
     * @return: java.util.List<java.lang.String>
     */
    List<String> getButtonPathByRedis();
    /**
     * @notes: 查询当前用户的按钮，键为前端页面标识path，唯一。值 0 false ，1 true
     * @Author: junyang.li
     * @Date: 11:29 2019/5/14
     * @return: java.util.Map<java.lang.String,java.lang.Integer>
     */
    Map<String, Object> getSelfButton();
}

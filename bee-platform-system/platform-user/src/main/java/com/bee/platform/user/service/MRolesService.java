package com.bee.platform.user.service;

import com.bee.platform.common.entity.MRoleInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.ManagerRoleType;
import com.bee.platform.user.dto.MRoleDTO;
import com.bee.platform.user.dto.MRoleGroupDTO;
import com.bee.platform.user.dto.MRoleListDTO;
import com.bee.platform.user.entity.MRoles;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.vo.ManagerRoleVO;

import java.util.List;
import java.util.Map;

/**
 * @notes 后台角色表
 * @Author junyang.li
 * @Date 14:38 2019/4/29
 **/
public interface MRolesService extends IService<MRoles> {
    /**
     * @notes  获得所有的角色
     * @Author junyang.li
     * @Date 14:34 2019/4/29
     **/
    List<MRoleInfo> getAllRoles();
    /**
     * @notes 通过roleIds查询roles
     * @Author junyang.li
     * @Date 14:46 2019/4/29
     **/
    List<MRoleInfo> getRolesByRoleIds(List<Integer> roleIds);
    /**
    * @notes: 通过roleIds查询roles 返回map对象 键是角色id，值是角色对象
    * @Author: junyang.li
    * @Date: 11:31 2019/5/9
    * @param roleIds :  角色id
    * @return: java.util.Map<java.lang.Integer,com.bee.platform.common.entity.MRoleInfo>
    */
    Map<Integer,MRoleInfo> searchRolesByRoleIds(List<Integer> roleIds);
    /**
     * @notes 通过roleId查询角色
     * @Author junyang.li
     * @Date 14:46 2019/4/29
     **/
    MRoleInfo getRoleByRoleId(Integer roleId);

    /**
     * @notes 权限配置权限列表查询
     * @Author junyang.li
     * @Date 16:32 2019/4/29
     **/
    ResponseResult<List<MRoleListDTO>> getPermissionGroup();
    /**
     * @notes 通过角色类型获得角色
     * @Author junyang.li
     * @Date 9:21 2019/4/30
     **/
    List<MRoleInfo> getRolesByType(ManagerRoleType roleType);

    /**
     * @notes: 根据角色id和权限类型获取角色
     * @Author: junyang.li
     * @Date: 14:01 2019/5/8
     * @param roleId : 角色id
     * @param type : 角色类型
     * @return: com.bee.platform.common.entity.MRoleInfo
     */
    MRoleInfo getRolesById(int roleId,ManagerRoleType type);
    /**
     * @notes: 通过角色id和角色类型查询角色，返回角色对象
     * @Author: junyang.li
     * @Date: 16:24 2019/5/8
     * @param roleIds : 角色id集
     * @param type : 角色类型
     * @return: java.util.List<com.bee.platform.common.entity.MRoleInfo>
     */
    List<MRoleInfo> getRolesByIds(List<Integer> roleIds,ManagerRoleType type);
    /**
     * @notes: 通过角色id和角色类型查询角色，返回角色id
     * @Author: junyang.li
     * @Date: 16:25 2019/5/8
     * @param roleIds : 角色id集
     * @param type : 角色类型
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> getRoleIdsByIds(List<Integer> roleIds,ManagerRoleType type);
    /**
     * @notes 删除角色组
     * @Author junyang.li
     * @Date 11:06 2019/4/30
     **/
    ResponseResult<ResCodeEnum> deleteRole(ManagerInfo managerInfo,Integer roleId);
    /**
     * @notes 编辑角色组
     * @Author junyang.li
     * @Date 13:51 2019/4/30
     **/
    ResponseResult<ResCodeEnum> editRole(ManagerInfo managerInfo, ManagerRoleVO vo);
    /**
     * @notes: 根据角色id查询角色权限，roleId 为空则返回所有的基础权限
     * @Author: junyang.li
     * @Date: 15:59 2019/5/8
     * @param roleId :
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.user.dto.MRoleGroupDTO>>
     */
    ResponseResult<List<MRoleGroupDTO>> getRoleDetail(Integer roleId);


    /**================ 扩展接口,暂未用到 =====================*/

    /**
     * @notes: 编辑基础角色的分组类别
     * @Author: junyang.li
     * @Date: 9:45 2019/5/13
     * @param managerInfo : 操作人信息
     * @param vo : 请求参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> updatePermission(ManagerInfo managerInfo,ManagerRoleVO vo);
}

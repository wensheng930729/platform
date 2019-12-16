package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthResourceDetailDTO;
import com.bee.platform.user.authority.entity.AuthResource;
import com.bee.platform.user.authority.rq.AuthResourceQueryRQ;
import com.bee.platform.user.authority.rq.AuthResourceRQ;

import java.io.InputStream;
import java.util.List;

/**
 * <p>
 * 资源表 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthResourceService extends IService<AuthResource> {
    /**
     * 获得所有资源-后台（分页）
     * @param pagination
     * @return
     */
    List<AuthResourceDetailDTO> listResourcesWithPage(AuthResourceQueryRQ rq, Pagination pagination);

    /**
     * 菜单树
     * @param resourcesList
     * @return
     */
    List<AuthResourceDetailDTO> getResourcesTree(List<AuthResourceDetailDTO> resourcesList);

    /**
     * 添加资源
     * @param userInfo
     * @param resourceRQ
     * @return
     */
    ResponseResult<ResCodeEnum> addResource(AuthPlatformUserInfo userInfo, AuthResourceRQ resourceRQ);

    /**
     * 删除资源
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> deleteResource(AuthPlatformUserInfo userInfo, String id);

    /**
     * 修改资源
     * @param userInfo
     * @param resourceRQ
     * @return
     */
    ResponseResult<ResCodeEnum> updateResource(AuthPlatformUserInfo userInfo, AuthResourceRQ resourceRQ);

    /**
     * 获取资源列表-用户
     * @param userInfo
     * @return
     */
    List<AuthResourceDetailDTO> listResourcesByUser(AuthPlatformUserInfo userInfo,String subSys);
    /**
     * @notes: 根据用户的子系统标识和角色id查询角色可访问的菜单
     * @Author: junyang.li
     * @Date: 11:15 2019/6/14
     * @param subSys : 子系统标识
     * @param roleId : 角色id
     * @param userInfo : 登录信息
     * @return: java.util.List<com.bee.platform.user.authority.dto.AuthResourceDetailDTO>
     */
    List<AuthResourceDetailDTO> getUserResourceByRoleId(String subSys,Integer roleId,AuthPlatformUserInfo userInfo);
    /**
     * 根据子系统查询所有资源-后台
     * @param userInfo
     * @param subSys
     * @return
     */
    List<AuthResourceDetailDTO> listResourcesBySubSys(AuthPlatformUserInfo userInfo, String subSys);
    /**
     * @notes: 批量导入资源
     * @Author: junyang.li
     * @Date: 15:13 2019/5/28
     * @param in :
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> excelImport(InputStream in);

    /**
     *
     * @param id
     * @return
     */
    ResponseResult<AuthResourceDetailDTO> getResourceDetail(String id);

    /**
     *
     * @param id
     * @return
     */
    List<AuthResourceDetailDTO> listSubResourceBackNoPage(String id);

    List<AuthResourceDetailDTO> resourcesByBackUser(AuthPlatformUserInfo userInfo);
}

package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.Enterprises;
import com.bee.platform.user.vo.UserManagementVO;

import java.util.List;
import java.util.Map;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface EnterprisesService extends IService<Enterprises> {
    
    ResponseResult<EnterpriseDetailDTO> getById(Integer id);
    
    List<Enterprises> listByNames(List<String> names);
    
    ResponseResult<List<Map>> assembleEnterpriseInfo(Integer userId);

    /**
     * 获取管理员信息
     * @param orgId
     * @return
     */
    List<AdminDTO> getAdmins(Integer orgId);

    /**
     * 添加管理员
     * @param orgId
     * @param id
     * @return
     */
    ResponseResult<String> addAdmins(Integer orgId, int id);

    /**
     * 批量添加管理员
     * @param orgId
     * @param old_ids
     * @param ids
     * @return
     */
    ResponseResult<String> addAdminsByList(Integer orgId, int[] old_ids, int[] ids);

    /**
     * 删除管理员
     * @param orgId
     * @param id
     * @return
     */
    ResponseResult<String> deleteAdmins(Integer orgId, int id);

    /**
     * 批量删除管理员
     * @param orgId
     * @param ids
     * @return
     */
    ResponseResult<String> deleteAdminsByList(Integer orgId, int[] ids);

    /**
     * 修改企业头像
     * @param orgId
     * @param head
     * @return
     */
    ResponseResult<String> modifyEnterpriseHead(Integer orgId, String head);

    /**
     * 转让超级管理员
     * @param userInfo
     * @return
     */
    ResponseResult<String> transferAdmin(AuthPlatformUserInfo userInfo, String phone);

    /**
     * @notes 获取当前用户的企业信息
     * @Author junyang.li
     * @Date 10:57 2019/3/18
     **/
    ResponseResult<List<EnterpriseListDTO>> getAllEnterprise(AuthPlatformUserInfo userInfo);
    /**
     * @notes 获取用户作为管理员的企业列表
     * @Author junyang.li
     * @Date 15:02 2019/3/18
     **/
    ResponseResult<List<EnterpriseListDTO>> getAllEnterpriseByAdmin(AuthPlatformUserInfo userInfo);
    /**
     * @notes 根据企业id查询企业下成员信息
     * @Author junyang.li
     * @Date 11:47 2019/3/20
     **/
    ResponseResult<List<UserManagementDTO>> listUsersByEnterpriseId(Integer orgId, UserManagementVO vo);
    /**
     * @notes 获得用户所在企业的基本信息
     * @Author junyang.li
     * @Date 14:16 2019/3/27
     **/
    ResponseResult<List<EnterpriseBasicDTO>> listBasicInfo(int userId);

    /**
     * @notes 查询是否是注册企业
     * @Author laixy
     * @Date 14:09 2019/4/29
     **/
    ResponseResult<List<EnterpriseInfoDTO>> listRegisteredEnterpriseInfo(String enterpriseName);

    /**
     * 企业申请名称校验
     * @param name 公司名称
     * @param userInfo 用户信息
     * @return 是否可注册
     */
    ResponseResult enterpriseCheck(AuthPlatformUserInfo userInfo, String name);

    /**
     * 模糊查询企业列表
     * @param name 公司名称
     * @param page 分页对象
     * @return 公司列表
     */
    ResponseResult<List<EnterpriseSearchDTO>> searchEnterpriseList(String name, Page page);
}

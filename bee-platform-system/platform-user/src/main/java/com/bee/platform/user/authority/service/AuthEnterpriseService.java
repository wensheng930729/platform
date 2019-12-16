package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.rq.*;
import com.bee.platform.user.dto.EnterpriseDetailDTO;
import com.bee.platform.user.dto.EnterpriseInfoDTO;
import com.bee.platform.user.dto.EnterpriseListDTO;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 企业表 服务类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthEnterpriseService extends IService<AuthEnterprise> {

    /**
     * 添加企业
     *
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> addEnterprise(AuthEnterpriseAddRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 修改企业信息
     *
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> updateEnterprise(AuthEnterpriseUpdateRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 删除企业
     *
     * @param rq
     * @return
     */
    ResponseResult delEnterprise(AuthEnterpriseDelRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 根据条件查询企业
     *
     * @param rq
     * @return
     */
    ResponseResult<List<AuthEnterpriseListDTO>> getByConditional(AuthEnterpriseGetRQ rq, AuthPlatformUserInfo userInfo, Page page);

    /**
     * 根据条件查询企业 （后台使用）
     *
     * @param rq
     * @param page
     * @return
     */
    ResponseResult<List<AuthEnterpriseListDTO>> getAllByConditional(AuthEnterpriseAllGetRQ rq, Page page);

    /**
     * 企业和应用、功能、企业和自定义功能 的关联关系都在一张表
     * 查询企业和应用功能详情
     *
     * @param enterpriseId
     * @return
     */
    ResponseResult<AuthEnterpriseDetailDTO> getEnterpriseDetail(Integer enterpriseId);

    /**
     * 根据企业ids查询企业基础信息详情
     *
     * @param orgIds
     * @return
     */
    ResponseResult<List<AuthEnterpriseFeignDetailDTO>> getEnterpriseMoreDetail(List<Integer> orgIds);

    /**
     * 查询所有的企业
     *
     * @return
     */
    ResponseResult<List<AuthEnterpriseAllDTO>> getAllEnterprise();

    /**
     * 查询所有企业树形结构 下拉列表使用
     *
     * @return
     */
    ResponseResult<List<AuthEnterpriseTreeDTO>> getEnterpriseTree();

    /**
     * 根据用户查询 用户企业树形结构 下拉列表使用 中台
     *
     * @param userInfo
     * @return
     */
    ResponseResult<AuthEnterpriseTreeDTO> getEnterpriseTreeByUser(AuthPlatformUserInfo userInfo);

    /**
     * 根据用户查询 用户企业 下拉列表使用
     *
     * @param userInfo
     * @return
     */
    ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByUser(AuthPlatformUserInfo userInfo);

    /**
     * 根据用户 查询企业及其父子企业
     *
     * @param userInfo
     * @return
     */
    ResponseResult<List<AuthEnterpriseFlatDTO>> getParentSubEnterpriseFlat(AuthPlatformUserInfo userInfo);

    /**
     * 子企业找祖宗
     */
    AuthEnterprise getAncestor(Integer enterpriseId);

    /**
     * 查询企业历史详情(后台)
     *
     * @param enterpriseId
     * @return
     */
    ResponseResult<AuthEnterpriseHistoryDTO> getEnterpriseHistory(Integer enterpriseId, AuthPlatformUserInfo userInfo);

    /**
     * 查询企业地址
     *
     * @param enterprise 企业信息
     * @param map        多个企业查询时 将区县id作为key 缓存到map
     * @return 企业地址的String 省市区+详细街道地址
     */
    public String getRegion(AuthEnterprise enterprise, Map<Integer, Map<String, Object>> map);

    /**
     * 根据当前企业查询所有上级公司id
     *
     * @param enterpriseId
     * @return
     */
    public List<Integer> getParentEnterpriseIds(Integer enterpriseId);

    /**
     * 根据用户id查询-用户所在企业及子企业
     *
     * @param userId
     * @return
     */
    ResponseResult<List<EnterpriseListDTO>> getUserEnterprises(Integer userId);
    /*------------------------------以前的接口-------------------------------*/

    /**
     * @param list :
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 15:44 2019/6/6
     * @return: void
     */
    void insertAll(List<AuthEnterprise> list);

    ResponseResult<AuthEnterpriseDTO> getParentEnterpriseId(Integer enterpriseId);

    ResponseResult<List<AuthPlatformUserDto>> getEnterpriseUsers(Integer userId);

    /*----------------------------以前的接口--------------------------*/

    /**
     * @notes 查询是否是注册企业
     * @Author laixy
     * @Date 14:09 2019/4/29
     **/
    ResponseResult<List<EnterpriseInfoDTO>> listRegisteredEnterpriseInfo(String enterpriseName);

    /**
     * 企业申请名称校验
     *
     * @param name     公司名称
     * @param userInfo 用户信息
     * @return 是否可注册
     */
    ResponseResult enterpriseCheck(AuthPlatformUserInfo userInfo, String name);

    /**
     * @notes 获取当前用户的企业信息
     * @Author junyang.li
     * @Date 10:57 2019/3/18
     **/
    ResponseResult<List<EnterpriseListDTO>> getAllEnterprise(AuthPlatformUserInfo userInfo);

    /**
     * 修改企业头像
     *
     * @param head
     * @return
     */
    ResponseResult<String> modifyEnterpriseHead(String head, AuthPlatformUserInfo userInfo);

    /**
     * 根据id查询企业
     *
     * @param orgId
     * @return
     */
    ResponseResult<EnterpriseDetailDTO> getById(Integer orgId);

    /**
     * 模糊查询企业列表
     *
     * @param name 公司名称
     * @return 公司列表
     */
    ResponseResult searchEnterpriseList(String name, Page page);

    /**
     * 根据企业id查询企业及子企业
     *
     * @param companyId
     * @return
     */
    ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByCompanyId(Integer companyId);
}

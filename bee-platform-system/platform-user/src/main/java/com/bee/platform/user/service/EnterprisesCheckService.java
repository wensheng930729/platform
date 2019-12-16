package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.rq.EnterpriseRegisterInfoRQ;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.EnterprisesCheck;

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
public interface EnterprisesCheckService extends IService<EnterprisesCheck> {
    /**
     * 企业注册
     * @param enterpriseRegisterInfoRQ
     * @return
     */
    ResponseResult<String> register(AuthPlatformUserInfo userInfo , EnterpriseRegisterInfoRQ enterpriseRegisterInfoRQ);
    /**
     * 获取企业信息
     * @param userInfo
     * @return
     */
    ResponseResult<EnterpriseWithAttacheDTO> getCompanyInfo(AuthPlatformUserInfo userInfo);
    /**
     * 修改企业信息
     * @param enterpriseRegisterInfoRQ
     * @return
     */
    ResponseResult<String> updateCompanyInfo(AuthPlatformUserInfo userInfo,EnterpriseRegisterInfoRQ enterpriseRegisterInfoRQ);
    /**
     * 查询用户申请认证还未通过的企业信息
     * @param phone
     * @return
     */
    List<EnterprisesCheck> getAuthenticatedList(String phone);
    /**
     * 通过企业审核表id查询企业审核详细信息
     * @param checkId
     * @return
     */
    EnterpriseCheckDTO getEnterpriseCheckInfo(Integer checkId);

    /**
     * 分页获取企业审核列表
     * @param pagination
     * @return
     */
    ResponseResult< Map<String,Object>> getAllEnterpriseCheckByPage(Pagination pagination);

    /**
     * 根据id查看详情
     * @param enterpriseId
     * @return
     */
    EnterprisesCheck getEnterpriseAllInfo(Integer enterpriseId);

//    /**
//     * 保存修改后的企业信息
//     * @param enterpriseInfoRQ
//     * @return
//     */
//    ResponseResult<String> updateEnterpriseInfo(EnterpriseInfoRQ enterpriseInfoRQ);

    /**
	 * 根据id查看企业审核详情
	 * @param id
	 * @return
	 */
	EnterpriseCheckDetailDTO getCheckInfo(Integer id);

    /**
     * 获取用户企业申请列表
     * @param userInfo 用户信息
     * @param page 分页对象
     * @return 企业申请列表
     */
    ResponseResult<List<UserApplyEnterprisesDTO>> getUserApplyList(AuthPlatformUserInfo userInfo, Page page);
	/**
	 * 审核企业
	 * @param id
	 * @param type
	 * @param reason
	 * @return
	 */
	ResponseResult audit(Integer id, Integer type, String reason,AuthPlatformUserInfo userInfo);

    /**
     * 根据id查询企业申请详情信息
     * @param id 企业审核id
     * @return 企业申请详情信心
     */
    EnterpriseRegisterInfoDTO getEnterpriseApplyDetail(Integer id);

   /* *//**
     * 变更企业管理员
     * @param userId 
     * @param userId
     * @return
     *//*
	ResponseResult modifyAdmin(Integer checkId, Integer userId, userInfo managerInfo);*/

}

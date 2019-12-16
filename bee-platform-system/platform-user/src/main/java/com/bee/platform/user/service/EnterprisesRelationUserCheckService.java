package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckAmendRQ;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckDTO;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckDetailsDTO;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckRQ;
import com.bee.platform.user.entity.EnterprisesRelationUserCheck;
import com.bee.platform.user.rq.EnterprisesRelationUserRQ;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 企业关联用户审核表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */
public interface EnterprisesRelationUserCheckService extends IService<EnterprisesRelationUserCheck> {

    /**
     * 企业关联用户
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return
     */
    ResponseResult enterprisesRelationUser(AuthPlatformUserInfo userInfo, EnterprisesRelationUserRQ rq);


    /**
     * 根据申请时间和电话号码和用户名查询企业审核申请表
     * @param nameOrPhone
     * @param startTime
     * @param endTime
     * @param pagination
     * @return
     */
	List<EnterprisesRelationUserCheckDTO> getApplyList(EnterprisesRelationUserCheckRQ rq,Integer enterpriseId,Pagination pagination);

	/**
	 * 根据ID修改企业关联审核
	 * @param Id
	 * @param checkStatus
	 * @param refusalReason
	 * @param userInfo
	 * @return
	 */
	ResponseResult<EnterprisesRelationUserCheck> enterprisesRelationUserCheckUpDate(EnterprisesRelationUserCheckAmendRQ rq,AuthPlatformUserInfo userInfo);

	/**
	 * 查看关联审核详情页
	 * @param id 关联审核表
	 * @param userInfo
	 * @return
	 */
	ResponseResult<EnterprisesRelationUserCheckDetailsDTO> getEnterprisesRelationUserCheckDetails(Integer id,
                                                                                                  AuthPlatformUserInfo userInfo);

	/**
	 * 用户关联企业名称校验
	 * @param request 请求
	 * @param name 公司名称
	 * @return 是否可关联
	 */
	ResponseResult enterpriseRelationCheck(HttpServletRequest request, String name);


	//ResponseResult<ArrayList<EnterprisesRelationUserCheckDTO>> getAllApplyList(Integer enterpriseId,Pagination pagination);









}

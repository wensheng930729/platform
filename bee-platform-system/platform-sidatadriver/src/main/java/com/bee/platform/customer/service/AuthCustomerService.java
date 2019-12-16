package com.bee.platform.customer.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.customer.dto.AuthCustomerBoxDto;
import com.bee.platform.customer.dto.AuthCustomerDetailDTO;
import com.bee.platform.customer.dto.AuthCustomerListDto;
import com.bee.platform.customer.dto.CustomerFirstCategoryBoxDto;
import com.bee.platform.customer.entity.AuthCustomer;
import com.bee.platform.customer.rq.AuthCustomerAddRQ;
import com.bee.platform.customer.rq.AuthCustomerSelectRQ;
import com.bee.platform.customer.rq.AuthCustomerUpdateRQ;

import java.util.List;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author hongchuan.He
 * @since 2019-05-20
 */
public interface AuthCustomerService extends IService<AuthCustomer> {

    /**
     * 添加客户
     *
     * @param rq
     * @return
     */
    ResponseResult<Integer> addCustomer(AuthCustomerAddRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 删除
     *
     * @param userInfo
     * @return
     */
    ResponseResult<Integer> deleteCustomer(Integer id, AuthPlatformUserInfo userInfo);

    /**
     * 更新客户信息
     *
     * @param rq
     * @return
     */
    ResponseResult<Integer> updateCustomer(AuthCustomerUpdateRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 禁用/启用客户
     *
     * @param id
     * @param status
     * @return
     */
    ResponseResult<Integer> updateStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo);
    /**
     * 根据客户id查询客户详情
     * @param customerId
     * @return
     */
    ResponseResult<AuthCustomerDetailDTO> getCustomerDetail(Integer customerId);
    /**
     * 条件查询客户
     *
     * @param rq
     * @param page
     * @return
     */
    ResponseResult<List<AuthCustomerListDto>> getList(AuthCustomerSelectRQ rq, Page page, Integer companyId);

    /**
     * 根据当前用户 (下拉列表查询用户企业下的客户使用)
     * @param pcode 客户一级分类
     * @param userInfo
     * @return
     */
    ResponseResult<List<AuthCustomerBoxDto>> getEnterpriseCustomer(String pcode, AuthPlatformUserInfo userInfo);

    /**
     * 根据企业id查询客户-下拉框
     * @param orgId
     * @return
     */
    ResponseResult<List<AuthCustomerBoxDto>> getCustomerByOrgId(Integer orgId);

    /**
     * 从erp码表查询客户一级分类
     * @return
     */
    ResponseResult<List<CustomerFirstCategoryBoxDto>> getCustomerFirstCatory();
}

package com.bee.platform.customer.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.customer.dto.ErpCustomerCategoryBoxDTO;
import com.bee.platform.customer.dto.ErpCustomerCategoryListDTO;
import com.bee.platform.customer.entity.ErpCustomerCategory;
import com.bee.platform.customer.rq.ErpCustomerCategoryAddRQ;
import com.bee.platform.customer.rq.ErpCustomerCategorySelectRQ;
import com.bee.platform.customer.rq.ErpCustomerCategoryUpdateRQ;

import java.util.List;

/**
 * <p>
 * 客户分类 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpCustomerCategoryService extends IService<ErpCustomerCategory> {

    /**
     * 添加客户分类
     *
     * @return
     */
    ResponseResult<Integer> addCustomerCategory(ErpCustomerCategoryAddRQ rq,AuthPlatformUserInfo userInfo);

    /**
     * 删除客户分类
     *
     * @return
     */
    ResponseResult<Integer> deleteCustomerCategory(Integer id, AuthPlatformUserInfo userInfo);

    /**
     * 修改客户分类
     *
     * @return
     */
    ResponseResult<Integer> updateCustomerCategory(ErpCustomerCategoryUpdateRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 修改客户启用/禁用状态
     *
     * @return
     */
    ResponseResult<Integer> updateSatatus(Integer id, Integer status, AuthPlatformUserInfo userInfo);

    /**
     * 客户分类列表
     *
     * @param companyId
     * @return
     */
    ResponseResult<List<ErpCustomerCategoryListDTO>> getList(ErpCustomerCategorySelectRQ rq, Integer companyId, Page page);
    /**
     * 通过分类类型，查询企业下的二级客户分类-下拉框使用
     * @param pcode
     * @param userInfo
     * @return
     */
    ResponseResult<List<ErpCustomerCategoryBoxDTO>> getEnterpriseTwoCategories(String pcode,AuthPlatformUserInfo userInfo);
    /**
     * 通过登录用户企业下的二级客户分类-下拉框使用
     * @param userInfo
     * @return
     */
    ResponseResult<List<ErpCustomerCategoryBoxDTO>> getUserTwoCategories(AuthPlatformUserInfo userInfo);
}

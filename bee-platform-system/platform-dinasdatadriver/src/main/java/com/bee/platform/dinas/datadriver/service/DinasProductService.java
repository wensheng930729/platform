package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.DinasProduct;
import com.bee.platform.dinas.datadriver.rq.DinasProductAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasProductQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasProductUpdateRQ;

import java.util.List;

/**
 * <p>
 * 产品表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasProductService extends IService<DinasProduct> {
    /**
     * 添加产品
     *
     * @param userInfo
     * @param rq
     * @return
     */
    public ResponseResult<Integer> addProduct(AuthPlatformUserInfo userInfo, DinasProductAddRQ rq);

    /**
     * 批量删除
     *
     * @param ids
     * @return
     */
    public ResponseResult<List<Integer>> deleteProduct(AuthPlatformUserInfo userInfo, List<Integer> ids);

    /**
     * 编辑
     *
     * @param userInfo
     * @param rq
     * @return
     */
    public ResponseResult<Integer> updateProduct(AuthPlatformUserInfo userInfo, DinasProductUpdateRQ rq);

    /**
     * 切换状态
     *
     * @param userInfo
     * @param id
     * @param status
     * @return
     */
    public ResponseResult<Integer> switchProduct(AuthPlatformUserInfo userInfo, Integer id, Integer status);

    /**
     * 详情
     *
     * @param id
     * @return
     */
    public ResponseResult<DinasProductDetailDTO> getProductDetail(Integer id);

    /**
     * 产品列表
     *
     * @param orgId
     * @param page
     * @param rq
     * @return
     */
    public ResponseResult<List<DinasProductList2DTO>> getProductList(Integer orgId, Page page, DinasProductQueryRQ rq);

    /**
     * 查询所有可关联产品
     *
     * @param orgId
     * @return
     */
    public ResponseResult<List<DinasProductSpecAllocateDTO>> getRelateProductAll(Integer orgId);

    /**
     * 查询客户已关联的产品-产品和规格
     *
     * @param customerId
     * @return
     */
    public ResponseResult<List<DinasProductSpecAllocateDTO>> getRelateProductCustomer(Integer customerId);

    /**
     * 查询当前用户企业下产品
     *
     * @param orgId
     * @return
     */
    public ResponseResult<List<DinasProductBoxDTO>> getCompanyProduct(Integer orgId);

    /**
     * 根据客户id查询客户关联产品
     *
     * @param customerId
     * @return
     */
    public ResponseResult<List<DinasProductBoxDTO>> getCustomerProduct(Integer customerId);

    /**
     * 根据客户id和产品id查询客户关联产品规格
     *
     * @param customerId
     * @param productId
     * @return
     */
    public ResponseResult<List<DinasProductSpecBoxDTO>> getCustomerProductSpec(Integer customerId, Integer productId);
    /**
     * 根据产品id查询产品规格
     *
     * @param productId
     * @return
     */
    public ResponseResult<List<DinasProductSpecBoxDTO>> getProductSpecByProduct(Integer productId);

}

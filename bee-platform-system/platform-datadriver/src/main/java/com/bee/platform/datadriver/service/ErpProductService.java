package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpProduct;
import com.bee.platform.datadriver.rq.ErpProductAddRQ;
import com.bee.platform.datadriver.rq.ErpProductListRQ;
import com.bee.platform.datadriver.rq.ErpProductUpdateRQ;

import java.util.List;

/**
 * <p>
 * 产品档案 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpProductService extends IService<ErpProduct> {

    /**
     * 增加erp产品
     *
     * @return
     */
    public ResponseResult<Integer> addProduct(ErpProductAddRQ rq,AuthPlatformUserInfo userInfo);
    /**
     * 删除erp产品
     *
     * @return
     */
    public ResponseResult<Integer> deleteProduct(Integer id,AuthPlatformUserInfo userInfo);
    /**
     * 修改erp产品
     *
     * @return
     */
    public ResponseResult<Integer> updateProduct(ErpProductUpdateRQ rq,AuthPlatformUserInfo userInfo);
    /**
     * 禁用/启用产品
     *
     * @param id
     * @param status
     * @return
     */
    ResponseResult<Integer> updateStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo);
    /**
     * erp产品列表
     *
     * @return
     */
    public ResponseResult<List<ErpProductListDTO>> getProductConditional(ErpProductListRQ rq, Page page, Integer companyId);
    /**
     * 根据id查询产品
     *
     * @return
     */
    public ResponseResult<ErpProductDetailDTO> getById(Integer id);

    /**
     * 当前用户所在企业的产品
     *
     * @return
     */
    ResponseResult<List<ErpProductBoxDTO>> getEnterpriseProduct(AuthPlatformUserInfo userInfo);
    /**
     * 当前用户所在企业及-子企业的产品
     *
     * @return
     */
    ResponseResult<List<ErpProductBoxDTO>> getEnterpriseSubProduct(AuthPlatformUserInfo userInfo);
    /**
     * 当前用户所在企业及-集团下所有公司的产品
     *
     * @return
     */
    ResponseResult<List<ErpProductBoxDTO>> getEnterpriseGroupProduct(AuthPlatformUserInfo userInfo);

    /**
     * 通过产品id获取产品名称和产品分类名称
     * @param productId
     * @return
     */
    ErpProductCategoryNameDTO getProductCategoryName(Integer productId);
    /**
     * 根据产品id获取 该产品的检测属性
     * @param id 产品id
     * @return
     */
    ResponseResult<List<ErpProductCategoryCheckItemsDTO>> getCheckItems(Integer id);


}

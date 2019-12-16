package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrderDetail;
import com.bee.platform.dinas.datadriver.rq.DinasOrderQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseOrderAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseOrderUpdateRQ;

import java.util.List;

/**
 * <p>
 * 采购合同 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasPurchaseOrderService extends IService<DinasPurchaseOrder> {

    /**
     * 分页查询订单列表
     * @param pagination
     * @param rq
     * @param userInfo
     * @return
     */
    List<DinasPurchaseOrderListDTO> listPurchaseOrder(Pagination pagination, DinasOrderQueryRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 获取订单详情
     * @param id
     * @return
     */
    DinasPurchaseOrderInfoDTO getPurchaseOrderDetail(Integer id);

    /**
     * 删除订单
     * @param userInfo
     * @param ids
     * @return
     */
    ResponseResult<List<Integer>> deletePurchaseOrder(AuthPlatformUserInfo userInfo, List<Integer> ids);

    /**
     * 新增订单
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> addPurchaseOrder(AuthPlatformUserInfo userInfo, DinasPurchaseOrderAddRQ rq);

    /**
     * 修改订单
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> updatePurchaseOrder(AuthPlatformUserInfo userInfo, DinasPurchaseOrderUpdateRQ rq);

    /**
     * 获取订单使用的产品
     * @param id
     * @return
     */
    ResponseResult<List<DinasProductListDTO>> getOrderProduct(Integer id);

    /**
     * 根据产品id获取采购订单使用的规格
     * @param orderId
     * @param productId
     * @return
     */
    ResponseResult<List<DinasProductSpecListDTO>> getOrderProductSpec(Integer orderId, Integer productId);

    /**
     * 查询采购订单下拉列表
     * @param orderId
     * @param productId
     * @param specId
     * @return
     */
    ResponseResult<DinasPurchaseOrderDetail> getPriceByProductSpec(Integer orderId, Integer productId, Integer specId);

    /**
     * 查询采购订单下拉列表
     * @param userInfo
     * @return
     */
    List<DinasPurchaseOrderPullListDTO> getOrderPullList(AuthPlatformUserInfo userInfo);
}

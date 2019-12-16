package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.enums.EnumSaleOrderStatus;
import com.bee.platform.datadriver.rq.OrderDeleteRQ;
import com.bee.platform.datadriver.rq.OrderQueryRQ;
import com.bee.platform.datadriver.rq.PurchaseOrderSaveRQ;

import java.util.List;

/**
 * <p>
 * 采购订单 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseOrderService extends IService<ErpPurchaseOrder> {

    /**
     * 分页查询采购订单列表
     * @param pagination
     * @param rq
     * @return
     */
    List<ErpPurchaseOrderInfoDTO> listErpPurchaseOrder(Pagination pagination, OrderQueryRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 获取采购订单信息
     * @param id
     * @return
     */
    ResponseResult<ErpPurchaseOrderDTO> getErpPurchaseOrder(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 保存采购订单
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> saveErpPurchaseOrder(AuthPlatformUserInfo userInfo, PurchaseOrderSaveRQ rq);

    /**
     * 统计采购订单状态信息
     * @param companyId
     * @return
     */
    ResponseResult<OrderStatusCountDTO> countErpPurchaseOrderStatu(Integer companyId);

    /**
     * 根据合同号获取采购订单信息
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<List<ErpPurchaseOrderInfoDetailDTO>> getErpPurchaseOrderById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据当前用户查询，当前用户公司及其子公司的采购订单合同编号
     * @param userInfo
     * @return
     */
    ResponseResult<List<ErpPucharseOrderBoxDTO>> getPucharseOrderBox(AuthPlatformUserInfo userInfo);

    /**
     * 获取采购订单信息
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<Integer> deleteErpPurchaseOrder(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 订单执行情况
     * @param id
     * @return
     */
    ResponseResult<ErpPurchaseOrderExcuteDTO> getErpPurchaseOrderExcute(Integer id);

    /**
     * 订单执行订单执行状态
     * @param id
     * @return
     */
    Integer getOrderExcuteStatu(Integer id);
}

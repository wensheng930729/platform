package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpSaleInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrder;
import com.bee.platform.datadriver.entity.ErpSaleInvoiceOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpSaleInvoiceOrderSaveRQ;
import com.bee.platform.datadriver.rq.InvoiceOrderQueryRQ;
import com.bee.platform.datadriver.rq.OrderDeleteRQ;

import java.util.List;

/**
 * <p>
 * 销售发票 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
public interface ErpSaleInvoiceOrderService extends IService<ErpSaleInvoiceOrder> {

    /**
     * 分页查询销售订单列表
     * @param pagination
     * @param rq
     * @return
     */
    List<ErpSaleInvoiceOrderInfoDTO> listSaleInvoiceOrder(Pagination pagination, InvoiceOrderQueryRQ rq, Integer companyId);

    /**
     * 获取销售发票信息
     * @param id
     * @return
     */
    ResponseResult<ErpSaleInvoiceOrder> getSaleInvoiceOrder(String id);

    /**
     * 删除销售发票信息
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> deleteSaleInvoiceOrder(AuthPlatformUserInfo userInfo, OrderDeleteRQ rq);

    /**
     * 保存销售发票
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> saveSaleInvoiceOrder(AuthPlatformUserInfo userInfo, ErpSaleInvoiceOrderSaveRQ rq);
}

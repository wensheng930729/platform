package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpPurchaseInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.rq.ErpPurchaseInvoiceOrderSaveRQ;
import com.bee.platform.datadriver.rq.InvoiceOrderQueryRQ;
import com.bee.platform.datadriver.rq.OrderDeleteRQ;

import java.util.List;

/**
 * <p>
 * 采购发票 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
public interface ErpPurchaseInvoiceOrderService extends IService<ErpPurchaseInvoiceOrder> {

    /**
     * 分页查询采购发票列表
     * @param pagination
     * @param rq
     * @return
     */
    List<ErpPurchaseInvoiceOrderInfoDTO> listPurchaseInvoiceOrder(Pagination pagination, InvoiceOrderQueryRQ rq, Integer companyId);

    /**
     * 获取采购发票信息
     * @param id
     * @return
     */
    ResponseResult<ErpPurchaseInvoiceOrder> getPurchaseInvoiceOrder(String id);

    /**
     * 删除采购发票信息
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> deletePurchaseInvoiceOrder(AuthPlatformUserInfo userInfo, OrderDeleteRQ rq);

    /**
     * 保存采购发票
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> savePurchaseInvoiceOrder(AuthPlatformUserInfo userInfo, ErpPurchaseInvoiceOrderSaveRQ rq);
}

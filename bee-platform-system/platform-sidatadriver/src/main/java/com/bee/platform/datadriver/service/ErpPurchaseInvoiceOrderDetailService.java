package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpInvoiceOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrderDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpInvoiceOrderDetailRQ;
import com.bee.platform.datadriver.rq.ErpSaleInvoiceOrderSaveRQ;

import java.util.List;

/**
 * <p>
 * 采购发票明细 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
public interface ErpPurchaseInvoiceOrderDetailService extends IService<ErpPurchaseInvoiceOrderDetail> {

    /**
     * 获取采购发票明细列表
     * @param id
     * @return
     */
    ResponseResult<List<ErpInvoiceOrderDetailDTO>> listPurchaseInvoiceOrderDetail(String id);

    /**
     * 删除采购发票明细信息
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> deletePurchaseInvoiceOrderDetail(AuthPlatformUserInfo userInfo, String id);

    /**
     * 保存采购发票明细发票
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> savePurchaseInvoiceOrderDetail(AuthPlatformUserInfo userInfo, ErpInvoiceOrderDetailRQ rq);
}

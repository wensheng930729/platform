package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpInvoiceOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpSaleInvoiceOrderDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpInvoiceOrderDetailRQ;

import java.util.List;

/**
 * <p>
 * 销售发票明细 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
public interface ErpSaleInvoiceOrderDetailService extends IService<ErpSaleInvoiceOrderDetail> {

    /**
     * 获取销售发票明细信息
     * @param id
     * @return
     */
    ResponseResult<List<ErpInvoiceOrderDetailDTO>> listSaleInvoiceOrderDetail(String id);

    /**
     * 删除销售发票明细信息
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> deleteSaleInvoiceOrderDetail(AuthPlatformUserInfo userInfo, String id);

    /**
     * 保存采购发票明细发票
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> saveSaleInvoiceOrderDetail(AuthPlatformUserInfo userInfo, ErpInvoiceOrderDetailRQ rq);
}

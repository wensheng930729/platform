package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseOrderDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.PurchaseOrderDetailSaveRQ;

import java.util.List;

/**
 * <p>
 * 采购单明细 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseOrderDetailService extends IService<ErpPurchaseOrderDetail> {

    /**
     * 分页查询采购订单明细列表
     * @param pagination
     * @param id
     * @return
     */
    List<ErpPurchaseOrderDetailDTO> listErpPurchaseOrderDetail(Pagination pagination, String id);

    /**
     * 删除采购订单明细
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> deleteErpPurchaseOrderDetail(AuthPlatformUserInfo userInfo, String id);

    /**
     * 保存采购订单明细
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> saveErpPurchaseOrderDetail(AuthPlatformUserInfo userInfo, PurchaseOrderDetailSaveRQ rq);
}

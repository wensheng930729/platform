package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.rq.DinasOrderQueryRQ;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 采购合同 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasPurchaseOrderMapper extends BaseMapper<DinasPurchaseOrder> {

    /**
     * 列表查询
     * @param rq
     * @param pagination
     * @return
     */
    List<DinasPurchaseOrderListDTO> listPurchaseOrderByCondition(DinasOrderQueryRQ rq, Pagination pagination);

    /**
     * 订单详细信息
     * @param id
     * @return
     */
    DinasPurchaseOrderInfoDTO getPurchaseOrderDetail(Integer id);

    /**
     * 获取订单使用的产品
     * @param id
     * @return
     */
    List<DinasProductListDTO> getOrderProduct(Integer id);

    /**
     * 根据产品id获取采购订单使用的规格
     * @param orderId
     * @param productId
     * @return
     */
    List<DinasProductSpecListDTO> getOrderProductSpec(@Param("orderId") Integer orderId, @Param("productId") Integer productId);

    /**
     * 查询采购订单下拉列表
     * @param orgId
     * @return
     */
    List<DinasPurchaseOrderPullListDTO> getOrderPullList(Integer orgId);
}

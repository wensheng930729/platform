package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.customer.dto.ErpSaleOrderDetailDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDetailDTO;
import com.bee.platform.datadriver.dto.ErpSaleOrderQueryDTO;
import com.bee.platform.datadriver.entity.ErpSaleOrderDetail;

import java.util.List;

import com.baomidou.mybatisplus.mapper.BaseMapper;

/**
 * <p>
 * 销售单明细 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpSaleOrderDetailMapper extends BaseMapper<ErpSaleOrderDetail> {

	List<ErpSaleOrderDetailDTO> selectErpSaleOrderDetail(int orderId);

	List<ErpSaleOrderQueryDTO> findSaleOrderInfo(Integer orderId);

}

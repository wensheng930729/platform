package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpSaleOrderListDTO;
import com.bee.platform.datadriver.dto.ErpSaleOrderQueryDTO;
import com.bee.platform.datadriver.entity.ErpSaleOrder;
import com.bee.platform.datadriver.rq.ErpSaleOrderListRQ;
import com.bee.platform.datadriver.rq.ErpSaleOrderQueryRQ;

import java.util.List;

/**
 * <p>
 * 销售单 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpSaleOrderMapper extends BaseMapper<ErpSaleOrder> {
	List<ErpSaleOrderQueryDTO> query(ErpSaleOrderQueryRQ erpSaleOrderQueryRQ,Pagination pagination);

	List<ErpSaleOrderListDTO> findSaleOrders(ErpSaleOrderListRQ saleOrderListRQ, Pagination pagination);
}

package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.dto.ErpLogisticsLogisticsTrackingDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsOrdersDTO;
import com.bee.platform.datadriver.entity.ErpLogisticsOrders;
import com.bee.platform.datadriver.rq.ErpLogisticsLogisticsTrackingRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersQueryRQ;

import java.util.List;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;

/**
 * <p>
 * 物流订单 Mapper 接口
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
public interface ErpLogisticsOrdersMapper extends BaseMapper<ErpLogisticsOrders> {

	List<ErpLogisticsOrdersDTO> query(ErpLogisticsOrdersQueryRQ rq, Pagination pagination);

	List<ErpLogisticsLogisticsTrackingDTO> queryStatus(ErpLogisticsLogisticsTrackingRQ rq, Pagination pagination);

}

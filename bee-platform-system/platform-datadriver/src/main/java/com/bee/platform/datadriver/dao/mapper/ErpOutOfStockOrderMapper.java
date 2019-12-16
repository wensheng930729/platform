package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderSearchListDTO;
import com.bee.platform.datadriver.entity.ErpOutOfStockOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpOutOfStockSearchRQ;

import java.util.List;

/**
 * <p>
 * 领料出库主表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpOutOfStockOrderMapper extends BaseMapper<ErpOutOfStockOrder> {


    List<ErpOutOfStockOrderSearchListDTO> searchOutOfStockOrderByCondition(ErpOutOfStockSearchRQ rq, Pagination pagination);


}

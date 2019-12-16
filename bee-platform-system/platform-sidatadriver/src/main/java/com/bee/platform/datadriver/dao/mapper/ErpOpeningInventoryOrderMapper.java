package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderSearchListDTO;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.OpeningInventoryOrderQueryRQ;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 期初库存主表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpOpeningInventoryOrderMapper extends BaseMapper<ErpOpeningInventoryOrder> {

    /**
     *
     * @param rq
     * @param pagination
     * @return
     */
    List<ErpOpeningInventoryOrderSearchListDTO> selectInventoryOrderByCondition(OpeningInventoryOrderQueryRQ rq, Pagination pagination);
}

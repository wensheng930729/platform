package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpStockSearchListDTO;
import com.bee.platform.datadriver.entity.ErpStock;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpStockSearchRQ;

import java.util.List;

/**
 * <p>
 * 库存表 Mapper 接口
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-03
 */
public interface ErpStockMapper extends BaseMapper<ErpStock> {


    List<ErpStockSearchListDTO> searchStockByCondition(ErpStockSearchRQ rq , Pagination pagination);
}

package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpStockCheckSearchDTO;
import com.bee.platform.datadriver.dto.ErpStockCheckSearchListDTO;
import com.bee.platform.datadriver.entity.ErpStockCheck;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpStockCheckSearchRQ;

import java.util.List;

/**
 * <p>
 * 库存盘点主单表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpStockCheckMapper extends BaseMapper<ErpStockCheck> {

    List<ErpStockCheckSearchListDTO> searchStockCheckByCondition(Pagination pagination, ErpStockCheckSearchRQ rq);


    List<ErpStockCheckSearchDTO> searchStockCheck(ErpStockCheckSearchRQ rq,Pagination pagination);
}

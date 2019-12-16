package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpProductBoxDTO;
import com.bee.platform.datadriver.dto.ErpWarehousingOrderSearchListDTO;
import com.bee.platform.datadriver.entity.ErpWarehousingOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpWarehousingOrderSearchRQ;

import java.util.List;

/**
 * <p>
 * 成品入库主表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpWarehousingOrderMapper extends BaseMapper<ErpWarehousingOrder> {

    ErpProductBoxDTO getProductByMaterialBatchId(Integer id);

    List<ErpWarehousingOrderSearchListDTO> searchWarehousingOrderByCondition(ErpWarehousingOrderSearchRQ rq, Pagination pagination);
}

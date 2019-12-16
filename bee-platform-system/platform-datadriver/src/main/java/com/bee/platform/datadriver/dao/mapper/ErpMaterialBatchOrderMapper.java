package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpMaterialBatchListDTO;
import com.bee.platform.datadriver.dto.ErpMaterialBatchSearchListDTO;
import com.bee.platform.datadriver.dto.ErpProductBoxDTO;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpMaterialBatchSearchRQ;
import org.springframework.data.repository.query.Param;

import java.util.List;

/**
 * <p>
 * 料批主表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpMaterialBatchOrderMapper extends BaseMapper<ErpMaterialBatchOrder> {

    List<ErpMaterialBatchListDTO> getMaterialBatchList(@Param("list") List<Integer> list);

    List<ErpProductBoxDTO> getMaterialBatchDetailProductList(Integer id);

    List<ErpMaterialBatchSearchListDTO> searchMaterialBatchByCondition(ErpMaterialBatchSearchRQ rq, Pagination pagination);
}

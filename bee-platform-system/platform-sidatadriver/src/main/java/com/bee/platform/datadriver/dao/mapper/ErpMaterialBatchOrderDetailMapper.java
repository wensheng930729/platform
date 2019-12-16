package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.dto.ErpMaterialBatchProductDTO;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrderDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 料批明细表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpMaterialBatchOrderDetailMapper extends BaseMapper<ErpMaterialBatchOrderDetail> {

    List<ErpMaterialBatchProductDTO> getMaterialBatchDetails(Integer id);

}

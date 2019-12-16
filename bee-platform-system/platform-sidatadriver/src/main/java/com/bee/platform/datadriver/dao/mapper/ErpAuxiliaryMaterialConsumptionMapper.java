package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpAuxiliaryMaterialConsumptionDTO;
import com.bee.platform.datadriver.entity.ErpAuxiliaryMaterialConsumption;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpAuxiliaryMaterialConsumptionSearchRQ;

import java.util.List;

/**
 * <p>
 * 辅材消耗表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpAuxiliaryMaterialConsumptionMapper extends BaseMapper<ErpAuxiliaryMaterialConsumption> {


    List<ErpAuxiliaryMaterialConsumptionDTO> searchAuxiliaryMaterialConsumptionByCondition(ErpAuxiliaryMaterialConsumptionSearchRQ rq, Pagination pagination);
}

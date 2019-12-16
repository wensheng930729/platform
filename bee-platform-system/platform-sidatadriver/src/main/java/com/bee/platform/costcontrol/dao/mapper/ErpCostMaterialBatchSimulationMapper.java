package com.bee.platform.costcontrol.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.costcontrol.entity.ErpCostMaterialBatchSimulation;
import com.bee.platform.costcontroller.dto.ErpCostMaterialSimulationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostMaterialSimulationListDTO;
import com.bee.platform.costcontroller.rq.ErpCostMaterialSimulationListRQ;

import java.util.List;

/**
 * <p>
 * 料批模拟 Mapper 接口
 * </p>
 *
 * @author xin.huang
 * @since 2019-06-25
 */
public interface ErpCostMaterialBatchSimulationMapper extends BaseMapper<ErpCostMaterialBatchSimulation> {

    List<ErpCostMaterialSimulationListDTO> findList(ErpCostMaterialSimulationListRQ rq, Pagination pagination);

    ErpCostMaterialSimulationDetailDTO findInfo (Integer id);

}

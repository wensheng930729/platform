package com.bee.platform.costcontrol.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.costcontrol.entity.ErpCostMaterialBatchSimulation;
import com.bee.platform.costcontroller.dto.ErpCostMaterialSimulationDTO;
import com.bee.platform.costcontroller.dto.ErpCostMaterialSimulationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostMaterialSimulationListDTO;
import com.bee.platform.costcontroller.dto.ErpCostMaterialSimulationResultDTO;
import com.bee.platform.costcontroller.rq.ErpCostMaterialSimulationListRQ;
import com.bee.platform.costcontroller.rq.ErpCostMaterialSimulationRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationResultStatusRQ;

import java.util.List;

/**
 * <p>
 * 料批模拟 服务类
 * </p>
 *
 * @author xin.huang
 * @since 2019-06-25
 */
public interface ErpCostMaterialBatchSimulationService extends IService<ErpCostMaterialBatchSimulation> {

    ResponseResult<ResCodeEnum> add(AuthPlatformUserInfo userInfo, ErpCostMaterialSimulationRQ rq);

    ResponseResult<ResCodeEnum> update(AuthPlatformUserInfo userInfo, ErpCostMaterialSimulationRQ rq);

    ResponseResult<ResCodeEnum> updateStatus(AuthPlatformUserInfo userInfo, ErpCostSimulationResultStatusRQ rq);

    ResponseResult<ResCodeEnum> deleteMaterial(Integer id);

    ResponseResult<List<ErpCostMaterialSimulationDTO>> findListByCompanyId(Integer companyId);

    ResponseResult<List<ErpCostMaterialSimulationListDTO>> findList(Integer companyId, ErpCostMaterialSimulationListRQ rq, Pagination pagination);

    ResponseResult<ErpCostMaterialSimulationDetailDTO> findInfo(Integer id);

    ResponseResult<ErpCostMaterialSimulationResultDTO> calculateResult(ErpCostMaterialSimulationRQ rq);
}

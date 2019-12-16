package com.bee.platform.costcontrol.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.costcontrol.entity.ErpCostSimulationCr;
import com.bee.platform.costcontroller.dto.ErpCostSimulationComputedResultDTO;
import com.bee.platform.costcontroller.dto.ErpCostSimulationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostSimulationQueryDTO;
import com.bee.platform.costcontroller.rq.ErpCostSimulationAddRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationCalculationRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationQueryRQ;
import com.bee.platform.costcontroller.rq.ErpCostSimulationUpdateRQ;

import java.util.List;

/**
 * <p>
 * 成本模拟基础配置 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
public interface ErpCostSimulationService extends IService<ErpCostSimulationCr> {

    /**
     * 添加成本模拟
     *
     * @param rq
     * @param userInfo
     * @return
     */
    public ResponseResult addCostSimulation(ErpCostSimulationAddRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 删除成本模拟
     *
     * @return
     */
    public ResponseResult deleteCostSimulation(Integer id);

    /**
     * 修改成本模拟
     *
     * @return
     */
    public ResponseResult updateCostSimulation(ErpCostSimulationUpdateRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 查询成本模拟列表
     *
     * @return
     */
    public ResponseResult<List<ErpCostSimulationQueryDTO>> getSimulationList(Integer companyId, ErpCostSimulationQueryRQ rq, AuthPlatformUserInfo userInfo, Page page);

    /**
     * 查询成本模拟单个详情
     *
     * @return
     */
    public ResponseResult<ErpCostSimulationDetailDTO> getCostSimulationDetail(Integer id);

    /**
     * 根据参数得出计算结果
     * @param rq
     * @return
     */
    public ResponseResult<ErpCostSimulationComputedResultDTO> getCalculationResults(ErpCostSimulationCalculationRQ rq);
}

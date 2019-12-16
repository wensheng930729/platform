package com.bee.platform.costcontrol.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.costcontrol.entity.ErpCostAllocationCr;
import com.bee.platform.costcontroller.dto.ErpCostAllocationBoxDTO;
import com.bee.platform.costcontroller.dto.ErpCostAllocationDetailDTO;
import com.bee.platform.costcontroller.dto.ErpCostAllocationQueryDTO;
import com.bee.platform.costcontroller.rq.ErpCostAllocationAddRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationQueryRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationSwitchRQ;
import com.bee.platform.costcontroller.rq.ErpCostAllocationUpdateRQ;

import java.util.List;

/**
 * <p>
 * erp成本小工具-成本配置 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
public interface ErpCostAllocationService extends IService<ErpCostAllocationCr> {

    /**
     * 添加erp成本配置
     *
     * @param rq
     * @param userInfo
     * @return
     */
    public ResponseResult addCostAllocation(ErpCostAllocationAddRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 更新成本配置
     *
     * @param rq
     * @return
     */
    public ResponseResult updateCostAllocation(ErpCostAllocationUpdateRQ rq);

    /**
     * 删除配置
     *
     * @return
     */
    public ResponseResult deleteCostAllocation(Integer id);

    /**
     * 切换配置的启用和禁用
     *
     * @param rq
     * @return
     */
    public ResponseResult updateStatus(ErpCostAllocationSwitchRQ rq);

    /**
     * 查询配置列表
     *
     *
     * @param companyId
     * @param rq
     * @param userInfo
     * @param page
     * @return
     */
    public ResponseResult<List<ErpCostAllocationQueryDTO>> getCostList(Integer companyId, ErpCostAllocationQueryRQ rq, AuthPlatformUserInfo userInfo, Page page);

    /**
     * 查询配置详情
     *
     * @param id
     * @return
     */
    public ResponseResult<ErpCostAllocationDetailDTO> getCostAllocationDetail(Integer id);

    /**
     * 查询配置的下拉框列表
     *
     * @param userInfo
     * @return
     */
    public ResponseResult<List<ErpCostAllocationBoxDTO>> getCostAllocationBox(AuthPlatformUserInfo userInfo);
    /**
     * 查询配置的下拉框列表
     *
     * @param companyId
     * @return
     */
    public ResponseResult<List<ErpCostAllocationBoxDTO>> getCostAllocationBoxByCompanyId(Integer companyId);
}

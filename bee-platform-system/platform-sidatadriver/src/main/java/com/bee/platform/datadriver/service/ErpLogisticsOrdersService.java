package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.datadriver.dto.ErpLogisticsLogisticsTrackingDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsLogisticsTrackingDetailDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsOrdersDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsOrdersDetailBatchIdDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsOrdersDetailDTO;
import com.bee.platform.datadriver.dto.OrderStatusCountDTO;
import com.bee.platform.datadriver.entity.ErpLogisticsOrders;
import com.bee.platform.datadriver.entity.ErpLogisticsOrdersDetail;
import com.bee.platform.datadriver.rq.ErpLogisticsLogisticsTrackingRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersAddRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersQueryRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsSettlementRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsShippingDetailsRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsStatusRQ;
import com.bee.platform.datadriver.rq.ErpUpdateLogisticsTrackingRQ;

import java.util.List;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 物流订单 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
public interface ErpLogisticsOrdersService extends IService<ErpLogisticsOrders> {

	ResponseResult<List<ErpLogisticsOrdersDTO>> query(Integer companyId, ErpLogisticsOrdersQueryRQ rq,
			Pagination pagination);
	
	ResponseResult addOrUpdate(AuthPlatformUserInfo userInfo, ErpLogisticsOrdersAddRQ rq);
	/**
	 * 物流订单修改状态
	 * @param userInfo
	 * @return
	 */
	ResponseResult update(AuthPlatformUserInfo userInfo, ErpLogisticsStatusRQ rq);

	/**
	 *  添加物流结算
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult addSettlement(AuthPlatformUserInfo userInfo, ErpLogisticsSettlementRQ rq);

	ResponseResult<List<ErpLogisticsLogisticsTrackingDTO>> queryStatus(AuthPlatformUserInfo userInfo,
			ErpLogisticsLogisticsTrackingRQ rq, Pagination pagination);

	ResponseResult updateDetails(AuthPlatformUserInfo userInfo, ErpLogisticsShippingDetailsRQ rq);

	/**
	 *  查询物流跟踪详情
	 * @param userInfo
	 * @param id
	 * @return
	 */
	ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> get(AuthPlatformUserInfo userInfo, Integer id);

	/**
	 * 订单状态统计
	 * @param orgId
	 * @return
	 */
    ResponseResult<OrderStatusCountDTO> countErpLogisticsOrdersStatu(Integer orgId);

	ResponseResult<List<ErpLogisticsOrdersDetailDTO>> queryOrdersDetail(Integer id);

	ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> getTrackingDetail(Integer id);

	ResponseResult<List<ErpLogisticsOrdersDetail>> queryOrders(Integer id);

	ResponseResult<List<ErpLogisticsOrdersDetail>> queryBatchIdOrProductId(Integer batchId, Integer id,
			Integer productId);

	ResponseResult<List<ErpLogisticsOrdersDetail>> queryBatchId(Integer id, Integer productId);

	ResponseResult<List<ErpLogisticsOrdersDetailBatchIdDTO>> queryOrdersDetailBatchIdDTO(Integer id);

	ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> getLogisticsOrders(AuthPlatformUserInfo userInfo,
			Integer id);

	ResponseResult<?> delete(AuthPlatformUserInfo userInfo, Integer id);

	ResponseResult<?> deleteLogisticsOrdersDetail(AuthPlatformUserInfo userInfo, Integer id);

	ResponseResult<?> updateLogisticsTracking(AuthPlatformUserInfo userInfo, ErpUpdateLogisticsTrackingRQ rq);

}

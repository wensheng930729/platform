package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.customer.dto.ErpSaleOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpSaleOrderDetail;
import com.bee.platform.datadriver.rq.ErpSaleOrderDetailAddRQ;

import java.util.List;

import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 销售单明细 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpSaleOrderDetailService extends IService<ErpSaleOrderDetail> {

	/**
	 * 删除销售订单详情
	 * @param userInfo
	 * @param id
	 * @return
	 */
	ResponseResult<Integer> deleteErpSaleOrderDetail(AuthPlatformUserInfo userInfo, int id);


	/**
	 * 编辑销售订单详情
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<Integer> updateErpSaleOrderDetail(AuthPlatformUserInfo userInfo, ErpSaleOrderDetailAddRQ rq);


	/**
	 * 查询销售订单详情
	 * @param userInfo
	 * @param orderId
	 * @return
	 */
	ResponseResult<List<ErpSaleOrderDetailDTO>> getErpSaleOrderDetail(AuthPlatformUserInfo userInfo, int orderId);


}

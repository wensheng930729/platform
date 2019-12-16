package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpSaleOrder;
import com.bee.platform.datadriver.rq.ErpSaleOrderListRQ;
import com.bee.platform.datadriver.rq.ErpSaleOrderQueryRQ;
import com.bee.platform.datadriver.rq.ErpSaleOrderUpdateRQ;

import java.util.List;

/**
 * <p>
 * 销售单 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpSaleOrderService extends IService<ErpSaleOrder> {

	/**
	 * 销售订单查询
	 * @param erpSaleOrderQueryRQ
	 * @param pagination
	 * @return
	 */
	ResponseResult<List<ErpSaleOrderQueryDTO>> query(Integer companyId , ErpSaleOrderQueryRQ erpSaleOrderQueryRQ, Pagination pagination);

	/**
	 * 编辑返回的数据
	 * @param id
	 * @return
	 */
	ResponseResult get(int id);

	/**
	 * 删除
	 * @param id
	 * @return
	 */
	ResponseResult<Integer> delete(Integer id);

	/**
	 * 编辑销售单
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, ErpSaleOrderUpdateRQ rq);

	/**
	 * 根据当前用户查询，当前用户公司及其子公司的销售订单合同编号
	 * @param userInfo
	 * @return
	 */
    ResponseResult<List<ErpSaleOrderBoxDTO>> getSaleOrderBox(AuthPlatformUserInfo userInfo, String sysToken);

	/**
	 * @Description 根据合同编号查询销售订单详情
	 * @Param id
	 * @Date 2019/6/2 17:39
	 * @Author xin.huang
	 * @Return
	 */
	ResponseResult<ErpSaleOrderInfoDTO> getSaleOrderInfo(Integer id);

	/**
	 * 根据订单id获取订单基本信息
	 * @param id
	 * @return
	 */
	ResponseResult<ErpSaleOrderQueryDTO> getSaleOrderBaseInfo(Integer id);

	/**
	 * @Description 销售订单列表
	 * @Param userInfo
	 * @Param saleOrderListRQ
	 * @Param pagination
	 * @Date 2019/6/10 15:45
	 * @Author xin.huang
	 * @Return
	 */
	ResponseResult<List<ErpSaleOrderListDTO>> findSaleOrders (Integer companyId, ErpSaleOrderListRQ saleOrderListRQ, Pagination pagination);

	/**
	 * @Description 统计销售订单状态列表
	 * @Param userInfo
	 * @Date 2019/6/10 16:28
	 * @Author xin.huang
	 * @Return
	 */
	ResponseResult<OrderStatusCountDTO> countErpSaleOrderStatus(Integer companyId);

	/**
	 * @Description 订单列表中根据明细id获取订单详情
	 * @Param id
	 * @Date 2019/6/10 18:32
	 * @Author xin.huang
	 * @Return
	 */
	ResponseResult<ErpSaleOrderListInfoDTO> findSaleOrderInfo(Integer id);

	/**
	 * @Description 查询销售订单执行情况
	 * @Param id
	 * @Date 2019/6/10 19:52
	 * @Author xin.huang
	 * @Return
	 */
	ResponseResult<ErpSaleOrderExcuteDTO> getErpSaleOrderExcute(Integer id);

}

package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpSaleOrderListRQ;
import com.bee.platform.datadriver.rq.ErpSaleOrderQueryRQ;
import com.bee.platform.datadriver.rq.ErpSaleOrderUpdateRQ;
import com.bee.platform.datadriver.service.ErpSaleOrderService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 销售单 前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@RequestMapping("/erpSaleOrder")
@Api(value = "ErpSaleOrderController",tags = "erp销售订单相关接口")
public class ErpSaleOrderController {
	@Autowired
	private AuthUserFeignClient userInfoFeignClient;
	
	@Autowired
	private ErpSaleOrderService erpSaleOrderService;

	@Autowired
	private UserInfoUtils userInfoUtils;

	@PostMapping("/query")
    @ApiOperation(value = "条件查询销售订单")
    public ResponseResult query(HttpServletRequest request, @RequestBody ErpSaleOrderQueryRQ erpSaleOrderQueryRQ,Page page) {
		Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
		if (ObjectUtils.isEmpty(companyId) && Objects.isNull(erpSaleOrderQueryRQ.getCompany())){
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
		if (ObjectUtils.isEmpty(simpleUserInfo)) {
			log.info("无法获取用户信息");
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
		Pagination pagination = PageUtils.transFromPage(page);
		return erpSaleOrderService.query(companyId, erpSaleOrderQueryRQ, pagination);
	}

	@GetMapping("/get")
    @ApiOperation(value = "查询销售订单详情")
    public ResponseResult get(@RequestParam int id) {
		return erpSaleOrderService.get(id);
	}
	
	@PostMapping("/delete")
    @ApiOperation(value = "根据id删除")
	@Log(businessType= EnumBusinessType.SALE, operateType = OperateType.DELETE)
    public ResponseResult<Integer> delete(Integer id) {
		return erpSaleOrderService.delete(id);
	}

	@PostMapping("/update")
    @ApiOperation(value = "添加/编辑销售单")
    public ResponseResult<Integer> update(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid ErpSaleOrderUpdateRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return erpSaleOrderService.update(userInfo, rq);
	}

	@GetMapping("/list")
    @ApiOperation(value = "获取当前用户所在公司及其子公司关联的销售订单列表")
    public ResponseResult<List<ErpSaleOrderBoxDTO>> getSaleOrderList(HttpServletRequest request) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient
				.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
				.getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		String sysToken = request.getHeader("sysToken");
		return erpSaleOrderService.getSaleOrderBox(userInfo,sysToken);
	}

	@GetMapping("/saleOrderInfo")
    @ApiOperation(value = "根据销售订单id查询订单详情")
    public ResponseResult<ErpSaleOrderInfoDTO> getSaleOrderInfo(Integer id) {
		return erpSaleOrderService.getSaleOrderInfo(id);
	}

	@PostMapping("/list/saleOrders")
	@ApiOperation(value = "销售订单列表")
	public ResponseResult findSaleOrders(HttpServletRequest request, @RequestBody ErpSaleOrderListRQ saleOrderListRQ, Page page) {
		Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
		if (ObjectUtils.isEmpty(companyId) && Objects.isNull(saleOrderListRQ.getCompany())){
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		AuthPlatformUserInfo simpleUserInfo = userInfoUtils.getSimpleUserInfo(request);
		if (ObjectUtils.isEmpty(simpleUserInfo)) {
			log.info("无法获取用户信息");
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
		Pagination pagination = PageUtils.transFromPage(page);
		return erpSaleOrderService.findSaleOrders(companyId, saleOrderListRQ, pagination);
	}

	@GetMapping("/countErpSaleOrderStatus")
	@ApiOperation(value = "统计销售订单状态信息")
	public ResponseResult<OrderStatusCountDTO> countErpSaleOrderStatus(HttpServletRequest request){
		Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));
		if (ObjectUtils.isEmpty(companyId)){
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return erpSaleOrderService.countErpSaleOrderStatus(companyId);
	}

	@GetMapping("/getErpSaleOrderExcute")
	@ApiOperation(value = "获取销售订单执行信息")
	public ResponseResult<ErpSaleOrderExcuteDTO> getErpSaleOrderExcute(Integer id){
		if (id == null) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return erpSaleOrderService.getErpSaleOrderExcute(id);
	}

	@GetMapping("/findSaleOrderInfo")
	@ApiOperation(value = "获取销售订单列表详情信息")
	public ResponseResult<ErpSaleOrderListInfoDTO> findSaleOrderInfo(Integer orderId){
		if (orderId == null) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return erpSaleOrderService.findSaleOrderInfo(orderId);
	}


	@GetMapping("/saleOrderBaseInfo")
	@ApiOperation(value = "根据销售订单id查询订单基本信息")
	public ResponseResult<ErpSaleOrderQueryDTO> getSaleOrderBaseInfo(Integer id) {
		return erpSaleOrderService.getSaleOrderBaseInfo(id);
	}
}


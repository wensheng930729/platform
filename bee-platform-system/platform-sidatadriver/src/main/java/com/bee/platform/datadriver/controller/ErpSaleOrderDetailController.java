package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpSaleOrderDetailAddRQ;
import com.bee.platform.datadriver.service.ErpSaleOrderDetailService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * <p>
 * 销售单明细 前端控制器
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-27
 */
@RestController
@RequestMapping("/erpSaleOrderDetail")
@Api(value = "ErpSaleOrderDetailController",tags = "erp销售订单明细相关接口")
public class ErpSaleOrderDetailController {

	@Autowired
	private AuthUserFeignClient userInfoFeignClient;
	
	@Autowired
	private ErpSaleOrderDetailService erpSaleOrderDetailService;
	
	@GetMapping("/getErpSaleOrderDetail")
    @ApiOperation(value = "查看销售订单详情")
    public ResponseResult getErpSaleOrderDetail(@RequestHeader("sysToken") String sysToken,int orderId) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,erpSaleOrderDetailService.getErpSaleOrderDetail(userInfo, orderId));

	}
	
	@PostMapping("/deleteErpSaleOrderDetail")
	@Log(businessType= EnumBusinessType.SALE, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除订单详情")
    public ResponseResult<Integer> deleteErpSaleOrderDetail(@RequestHeader("sysToken") String sysToken,int id) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return erpSaleOrderDetailService.deleteErpSaleOrderDetail(userInfo, id);
		
	}
	
	@PostMapping("/updateErpSaleOrderDetail")
    @ApiOperation(value = "添加/修改销售订单详情")
    public ResponseResult<Integer> updateErpSaleOrderDetail(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ErpSaleOrderDetailAddRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(sysToken).getObject();
		return erpSaleOrderDetailService.updateErpSaleOrderDetail(userInfo, rq);
		
	}
}


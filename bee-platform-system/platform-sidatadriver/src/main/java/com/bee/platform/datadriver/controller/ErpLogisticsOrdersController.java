package com.bee.platform.datadriver.controller;


import java.util.List;
import java.util.Objects;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpLogisticsLogisticsTrackingDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsLogisticsTrackingDetailDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsOrdersDTO;
import com.bee.platform.datadriver.dto.ErpLogisticsOrdersDetailBatchIdDTO;
import com.bee.platform.datadriver.dto.OrderStatusCountDTO;
import com.bee.platform.datadriver.entity.ErpLogisticsOrdersDetail;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpLogisticsInvoiceRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsLogisticsTrackingRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersAddRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersQueryRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsPaymentRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsSettlementRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsShippingDetailsRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsStatusRQ;
import com.bee.platform.datadriver.rq.ErpUpdateLogisticsTrackingRQ;
import com.bee.platform.datadriver.service.ErpLogisticsInvoiceService;
import com.bee.platform.datadriver.service.ErpLogisticsOrdersDetailService;
import com.bee.platform.datadriver.service.ErpLogisticsOrdersService;
import com.bee.platform.datadriver.service.ErpLogisticsPaymentService;
import com.bee.platform.datadriver.service.ErpLogisticsStatusDetailService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 物流订单 前端控制器
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpLogisticsOrders", tags = "erp物流相关接口")
@RequestMapping("/erpLogisticsOrders")
public class ErpLogisticsOrdersController {
	 @Autowired
	 private AuthUserFeignClient userInfoFeignClient;
	 
	 @Autowired
	 private ErpLogisticsOrdersService erpLogisticsOrdersService;
	 
	 @Autowired
	 private ErpLogisticsInvoiceService erpLogisticsInvoiceService;
	 
	 @Autowired
	 private ErpLogisticsOrdersDetailService erpLogisticsOrdersDetailService;
	 
	 @Autowired
	 private ErpLogisticsPaymentService erpLogisticsPaymentService;

	 
	 @PostMapping("/query")
	 @ApiOperation(value = "物流订单分页查询物流订单列表")
	 public ResponseResult<List<ErpLogisticsOrdersDTO>> query(HttpServletRequest request,@RequestBody ErpLogisticsOrdersQueryRQ rq, Page page){
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		 Pagination pagination = PageUtils.transFromPage(page);
		 List<ErpLogisticsOrdersDTO> list = erpLogisticsOrdersService.query(userInfo.getOrgId(), rq, pagination).getObject();
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
		 
	 }
	 
	 @Log(businessType = EnumBusinessType.LOGISTICS_ORDERS, operateType = OperateType.EDIT)
	 @PostMapping("/addOrUpdate")
	 @ApiOperation(value = "物流订单添加和编辑")
	 public ResponseResult<?> addOrUpdate(HttpServletRequest request, @RequestBody @Valid ErpLogisticsOrdersAddRQ rq) {
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		return erpLogisticsOrdersService.addOrUpdate(userInfo, rq);
		 
	 }
	 
	 @Log(businessType = EnumBusinessType.LOGISTICS_ORDERS, operateType = OperateType.ADD)
	 @PostMapping("/addOrders")
	 @ApiOperation(value = "物流订单新增")
	 public ResponseResult<?> addOrders(HttpServletRequest request, @RequestBody @Valid ErpLogisticsOrdersAddRQ rq) {
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		return erpLogisticsOrdersService.addOrUpdate(userInfo, rq);
		 
	 }
	 
		/**
		 * 删除物流订单明细
		 */
	 @Log(businessType = EnumBusinessType.LOGISTICS_ORDERS, operateType = OperateType.DELETE)
	 @PostMapping("/deleteLogisticsOrdersDetail")
	 @ApiOperation(value = "删除物流订单明细")
	public ResponseResult<?> deleteLogisticsOrdersDetail(HttpServletRequest request, @RequestParam Integer id){
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (null == id) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		return erpLogisticsOrdersService.deleteLogisticsOrdersDetail(userInfo, id);
		 
	 }
	 
	 
	 /**
		 * 逻辑删除物流订单状态明细
		 */
	 @Log(businessType = EnumBusinessType.LOGISTICS_ORDERS, operateType = OperateType.DELETE)
	 @PostMapping("/deleteStatus")
	 @ApiOperation(value = "物流状态明细删除")
	public ResponseResult<?> deleteStatus(HttpServletRequest request, @RequestParam Integer id){
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (null == id) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		return erpLogisticsOrdersService.delete(userInfo,id);
			
		}
	 
	 @Log(businessType = EnumBusinessType.LOGISTICS_ORDERS_STATUS, operateType = OperateType.EDIT)
	 @PostMapping("/update")
	 @ApiOperation(value = "修改物流订单状态")
	 public ResponseResult<?> update(HttpServletRequest request, @RequestBody @Valid ErpLogisticsStatusRQ rq) {
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		 return erpLogisticsOrdersService.update(userInfo, rq);
		 
	 }
	 
	 @Log(businessType = EnumBusinessType.LOGISTICS_SETTLEMENT, operateType = OperateType.EDIT)
	 @PostMapping("/addSettlement")
	 @ApiOperation(value = "添加物流结算")
	 public ResponseResult<?> addSettlement(HttpServletRequest request, @RequestBody @Valid ErpLogisticsSettlementRQ rq) {
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		return erpLogisticsOrdersService.addSettlement(userInfo, rq);
		 
	 }
	 
	 @PostMapping("/queryStatus")
	 @ApiOperation(value = "查询物流跟踪列表")
	 public ResponseResult<List<ErpLogisticsLogisticsTrackingDTO>> queryStatus(HttpServletRequest request, @RequestBody ErpLogisticsLogisticsTrackingRQ rq, Page page){
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		 Pagination pagination = PageUtils.transFromPage(page);
		 List<ErpLogisticsLogisticsTrackingDTO> list = erpLogisticsOrdersService.queryStatus(userInfo, rq, pagination).getObject();
		 return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
		 
	 }
	 
	 @Log(businessType = EnumBusinessType.LOGISTICS_TAIL_AFTER, operateType = OperateType.EDIT)
	 @PostMapping("/updateDetails")
	 @ApiOperation(value = "修改物流跟踪明细")
	 public ResponseResult<?> updateDetails(HttpServletRequest request, @RequestBody @Valid ErpLogisticsShippingDetailsRQ rq){
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		 // 更新xxx
		 return erpLogisticsOrdersService.updateDetails(userInfo, rq);
	 }
	 
	 
	 @GetMapping("/get")
	 @ApiOperation(value = "查询物流跟踪详情")
	 public ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> get(HttpServletRequest request, @RequestParam Integer id){
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (Objects.isNull(id)) {
			 log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		 return erpLogisticsOrdersService.get(userInfo, id);
	 }
	 
	 @GetMapping("/getLogisticsOrders")
	 @ApiOperation(value = "查询物流订单详情")
	 public ResponseResult<ErpLogisticsLogisticsTrackingDetailDTO> getLogisticsOrders(HttpServletRequest request,@RequestParam Integer id){
		 AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (Objects.isNull(id)) {
			 log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		 
		 return erpLogisticsOrdersService.getLogisticsOrders(userInfo, id);
	 }

	@GetMapping("/countErpLogisticsOrdersStatu")
	@ApiOperation(value = "统计物流订单状态信息")
	public ResponseResult<OrderStatusCountDTO> countErpLogisticsOrdersStatu(HttpServletRequest request){
		AuthPlatformUserInfo userInfo = userInfoFeignClient
				.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request))
				.getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		return erpLogisticsOrdersService.countErpLogisticsOrdersStatu(userInfo.getOrgId());
	}
	
	 
	@Log(businessType = EnumBusinessType.LOGISTICS_INVOICE, operateType = OperateType.ADD)
	@PostMapping("/add")
	@ApiOperation(value = "添加物流发票")
	public ResponseResult<?> add(HttpServletRequest request, @RequestBody @Valid ErpLogisticsInvoiceRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		 
		return erpLogisticsInvoiceService.add(userInfo, rq);
		
	}
	
	@Log(businessType = EnumBusinessType.LOGISTICS_INVOICE, operateType = OperateType.EDIT)
	@PostMapping("/updateErpLogisticsInvoice")
	@ApiOperation(value = "修改物流发票")
	public ResponseResult<?> updateErpLogisticsInvoice(HttpServletRequest request, @RequestBody @Valid ErpLogisticsInvoiceRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (ObjectUtils.isEmpty(rq)) {
	            log.info("参数错误");
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		 
		return erpLogisticsInvoiceService.update(userInfo, rq);
		
	}
	
	@Log(businessType = EnumBusinessType.LOGISTICS_INVOICE, operateType = OperateType.DELETE)
	@PostMapping("/deleteInvoiceDetailById")
	@ApiOperation(value = "根据ID删除物流发票明细")
	public ResponseResult<?> deleteInvoiceDetailById(HttpServletRequest request, @RequestParam(name = "id") Integer id) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		 if (ObjectUtils.isEmpty(userInfo)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
	        }
		 if (null == id) {
	            log.info("参数错误=>id", id);
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
	        }
		 
		return erpLogisticsInvoiceService.delete(userInfo, id);
		
	}
	
	
	
	@GetMapping("/queryOrdersDetail")
	@ApiOperation(value = "修改物流订单状态的时候返回数据列表")
	public ResponseResult<?> queryOrdersDetail(@RequestParam Integer id){
		return erpLogisticsOrdersService.queryOrdersDetail(id);
	}
	
	@GetMapping("/getTrackingDetail")
	@ApiOperation(value = "编辑需要返回物流订单")
	public ResponseResult<?> getTrackingDetail(@RequestParam Integer id){
		return erpLogisticsOrdersService.getTrackingDetail(id);
		
	}
	
	 /**
		 * 物流订单id返回产品id数据列表
		 */
	@GetMapping("/queryOrders")
	@ApiOperation(value = "物流订单id返回产品id数据列表")
	public ResponseResult<List<ErpLogisticsOrdersDetail>> queryOrders(Integer id){
		return erpLogisticsOrdersService.queryOrders(id);
	}
	 /**
		 * 物流订单产品批次返回数据列表
		 */
	@GetMapping("/queryBatchId")
	@ApiOperation(value = "物流订单产品批次返回数据列表")
	public ResponseResult<List<ErpLogisticsOrdersDetail>> queryBatchId(Integer id, Integer productId){
		return erpLogisticsOrdersService.queryBatchId(id, productId);
	    }
	
	@GetMapping("/queryBatchIdOrProductId")
	@ApiOperation(value = "物流订单产品批次返回数据")
	public ResponseResult<List<ErpLogisticsOrdersDetail>> queryBatchIdOrProductId(Integer batchId ,Integer id, Integer productId){
		return erpLogisticsOrdersService.queryBatchIdOrProductId(batchId, id, productId);
		
	}
	
	@GetMapping("/queryOrdersDetailBatchIdDTO")
	@ApiOperation(value = "添加物流订单状态返回数据")
    public ResponseResult<List<ErpLogisticsOrdersDetailBatchIdDTO>> queryOrdersDetailBatchIdDTO(Integer id){
		return erpLogisticsOrdersService.queryOrdersDetailBatchIdDTO(id);
	}

	/**
	 *  添加物流付款
	 */
	@Log(businessType = EnumBusinessType.LOGISTICS_PAYMENT, operateType = OperateType.ADD)
	@PostMapping("/addPay")
	@ApiOperation(value = "添加物流付款")
	public ResponseResult<?> addPay(HttpServletRequest request, @RequestBody @Valid ErpLogisticsPaymentRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return erpLogisticsPaymentService.add(userInfo, rq);
		
	}
	
	
	/**
	 * 编辑物流付款
	 */
	@Log(businessType = EnumBusinessType.LOGISTICS_PAYMENT, operateType = OperateType.EDIT)
	@PostMapping("/updatePay")
	@ApiOperation(value = "编辑物流付款")
	public ResponseResult<?> updatePay(HttpServletRequest request,@RequestBody @Valid ErpLogisticsPaymentRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return erpLogisticsPaymentService.updatePay(userInfo, rq);
	}
	
	
	/**
     * 编辑物流跟踪
     */
	@Log(businessType = EnumBusinessType.LOGISTICS_TAIL_AFTER, operateType = OperateType.EDIT)
	@PostMapping("/updateLogisticsTracking")
	@ApiOperation(value = "编辑物流跟踪")
    public ResponseResult<?> updateLogisticsTracking(HttpServletRequest request,@RequestBody @Valid ErpUpdateLogisticsTrackingRQ rq){
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return erpLogisticsOrdersService.updateLogisticsTracking(userInfo, rq);
		
	}
	
	
	 /**
     * 删除物流付款单
     */
	@Log(businessType = EnumBusinessType.LOGISTICS_PAYMENT, operateType = OperateType.DELETE)
	@PostMapping("/deletePayment")
	@ApiOperation(value = "删除物流付款单")
    public ResponseResult<?> deletePayment(HttpServletRequest request, @RequestParam Integer id) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(id)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return erpLogisticsPaymentService.deletePayment(userInfo, id);
		
	}
}


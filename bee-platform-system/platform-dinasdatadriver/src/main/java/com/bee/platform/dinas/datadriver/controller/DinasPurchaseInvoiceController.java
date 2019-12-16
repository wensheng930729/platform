package com.bee.platform.dinas.datadriver.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.dinas.datadriver.dto.DinasPurchaseInvoiceDTO;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceDeleteBatchRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceRQ;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseInvoiceService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.bee.platform.dinas.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 采购发票 前端控制器
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "dinasPurchaseInvoice", tags = "砂石采购发票相关接口")
@RestController
@RequestMapping("/dinasPurchaseInvoice")
public class DinasPurchaseInvoiceController {

	@Autowired
	private AuthUserFeignClient userInfoFeignClient;
	
	@Autowired
	private DinasPurchaseInvoiceService dinasPurchaseInvoiceService;
	
	@Log(businessType = EnumBusinessType.DINASPURCHASEINVOICE, operateType = OperateType.ADD)
	@PostMapping("/addInvoice")
	@ApiOperation(value = "采购发票添加")
	public ResponseResult<?> addInvoice(HttpServletRequest request, @RequestBody DinasPurchaseInvoiceRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchaseInvoiceService.addInvoice(userInfo, rq);
	}

	@PostMapping("/queryList")
	@ApiOperation(value = "采购发票单分页查询列表")
	public ResponseResult<List<DinasPurchaseInvoiceDTO>> queryList(HttpServletRequest request,
			@RequestBody DinasPurchaseInvoiceQueryRQ rq, Page page) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		Pagination pagination = PageUtils.transFromPage(page);
		return dinasPurchaseInvoiceService.queryList(userInfo, rq, pagination);

	}

	@PostMapping("/queryOne")
	@ApiOperation(value = "采购发票单明细")
	public ResponseResult<DinasPurchaseInvoiceDTO> queryOne(HttpServletRequest request, @RequestParam Integer id) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(id)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchaseInvoiceService.queryOne(userInfo, id);
	}

	@Log(businessType = EnumBusinessType.DINASPURCHASEINVOICE, operateType = OperateType.EDIT)
	@PostMapping("/updatePurchaseInvoice")
	@ApiOperation(value = "采购发票编辑")
	public ResponseResult<?> updatePurchaseInvoice(HttpServletRequest request, @RequestBody DinasPurchaseInvoiceRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchaseInvoiceService.updatePurchaseInvoice(userInfo, rq);
	}

	@Log(businessType = EnumBusinessType.DINASPURCHASEINVOICE, operateType = OperateType.DELETE)
	@PostMapping("/deletePurchaseInvoice")
	@ApiOperation(value = "删除采购发票")
	public ResponseResult<?> deletePurchaseInvoice(HttpServletRequest request, @RequestParam Integer id) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(id)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchaseInvoiceService.deletePurchaseInvoice(userInfo, id);

	}

	@Log(businessType = EnumBusinessType.DINASPURCHASEINVOICE, operateType = OperateType.DELETE, isBatch = true)
	@PostMapping("/deleteBatch")
	@ApiOperation(value = "删除采购发票列表")
	public ResponseResult<?> deleteBatch(HttpServletRequest request,
			@RequestBody DinasPurchaseInvoiceDeleteBatchRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchaseInvoiceService.deleteBatch(userInfo, rq);
	}
}

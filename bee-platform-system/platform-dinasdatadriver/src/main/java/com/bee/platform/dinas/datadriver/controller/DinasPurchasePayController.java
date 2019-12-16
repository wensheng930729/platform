package com.bee.platform.dinas.datadriver.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.springframework.web.bind.annotation.RestController;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.dinas.datadriver.dto.DinasPurchasePayDTO;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayDeleteBatchRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayRQ;
import com.bee.platform.dinas.datadriver.service.DinasPurchasePayService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.bee.platform.dinas.datadriver.support.annotation.Log;
import com.bee.platform.user.service.feign.AuthUserFeignClient;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 采购付款单 前端控制器
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "dinasPurchasePay", tags = "砂石采购付款相关接口")
@RestController
@RequestMapping("/dinasPurchasePay")
public class DinasPurchasePayController {

	@Autowired
	private DinasPurchasePayService dinasPurchasePayService;
	
	@Autowired
	private AuthUserFeignClient userInfoFeignClient;
	
	@Log(businessType = EnumBusinessType.DINASPURCHASEPAY, operateType = OperateType.ADD)
	@PostMapping("/addPurchasePay")
	@ApiOperation(value = "采购付款单添加")
	public ResponseResult<?> addPurchasePay(HttpServletRequest request, @RequestBody DinasPurchasePayRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchasePayService.addPurchasePay(userInfo, rq);
	}

	@PostMapping("/queryList")
	@ApiOperation(value = "采购付款单分页查询列表")
	public ResponseResult<List<DinasPurchasePayDTO>> queryList(HttpServletRequest request,
			@RequestBody DinasPurchasePayQueryRQ rq, Page page) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		Pagination pagination = PageUtils.transFromPage(page);
		return dinasPurchasePayService.queryList(userInfo, rq, pagination);
	}

	@PostMapping("/queryOne")
	@ApiOperation(value = "采购付款单明细")
	public ResponseResult<DinasPurchasePayDTO> queryOne(HttpServletRequest request, @RequestParam Integer id) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(id)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchasePayService.queryOne(userInfo, id);
	}

	@Log(businessType = EnumBusinessType.DINASPURCHASEPAY, operateType = OperateType.EDIT)
	@PostMapping("/updatePurchasePay")
	@ApiOperation(value = "采购付款单编辑")
	public ResponseResult<?> updatePurchasePay(HttpServletRequest request, @RequestBody DinasPurchasePayRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchasePayService.updatePurchasePay(userInfo, rq);
	}

	@Log(businessType = EnumBusinessType.DINASPURCHASEPAY, operateType = OperateType.DELETE)
	@PostMapping("/deletePurchasePay")
	@ApiOperation(value = "删除采购付款单")
	public ResponseResult<?> deletePurchasePay(HttpServletRequest request, @RequestParam Integer id) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(id)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchasePayService.deletePurchasePay(userInfo, id);

	}

	@Log(businessType = EnumBusinessType.DINASPURCHASEPAY, operateType = OperateType.DELETE, isBatch = true)
	@PostMapping("/deleteBatch")
	@ApiOperation(value = "批量删除采购付款单")
	public ResponseResult<?> deleteBatch(HttpServletRequest request, @RequestBody DinasPurchasePayDeleteBatchRQ rq) {
		AuthPlatformUserInfo userInfo = userInfoFeignClient.simpleUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request)).getObject();
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		if (ObjectUtils.isEmpty(rq)) {
			log.info("参数错误");
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		return dinasPurchasePayService.deleteBatch(userInfo, rq);
	}
	
}

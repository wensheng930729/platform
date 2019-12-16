package com.bee.platform.dinas.datadriver.service.impl;

import com.bee.platform.dinas.datadriver.entity.DinasCustomer;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrder;
import com.bee.platform.dinas.datadriver.entity.DinasPurchasePay;
import com.bee.platform.dinas.datadriver.entity.DinasSaleOrder;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayDeleteBatchRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasCustomerMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasPurchaseOrderMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasPurchasePayMapper;
import com.bee.platform.dinas.datadriver.dto.DinasPurchasePayDTO;
import com.bee.platform.dinas.datadriver.dto.DinasUrlDTO;
import com.bee.platform.dinas.datadriver.service.DinasPurchasePayService;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;

import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * <p>
 * 采购付款单 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasPurchasePayServiceImpl extends ServiceImpl<DinasPurchasePayMapper, DinasPurchasePay> implements DinasPurchasePayService {

	@Autowired
	private DinasPurchasePayMapper dinasPurchasePayMapper;
	
	@Autowired
	private DinasPurchaseOrderMapper dinasPurchaseOrderMapper;
	
	@Autowired
	private DinasCustomerMapper dinasCustomerMapper;
	
	@Autowired
    private AppendixUtils appendixUtils;

	/**
	 * 添加采购付款
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> addPurchasePay(AuthPlatformUserInfo userInfo, DinasPurchasePayRQ rq) {
		DinasPurchasePay dinasPurchasePay = BeanUtils.copyProperties(rq, DinasPurchasePay.class);
		if (CollectionUtils.isNotEmpty(rq.getList())) {
			dinasPurchasePay.setCreateUser(userInfo.getId())
					.setCreateTime(new Date())
					.setDeleted(Status.FALSE.getKey())
					.setPayDate(rq.getPayDate()).setCompanyId(userInfo.getOrgId()).setCompanyName(userInfo.getOrg_name())
					.setUrl(appendixUtils.getJsonStr(rq.getList()));
			if (dinasPurchasePayMapper.insert(dinasPurchasePay)<0) {
				return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
			}
		}else {
			dinasPurchasePay.setCreateUser(userInfo.getId())
					.setCreateTime(new Date()).setCompanyId(userInfo.getOrgId()).setCompanyName(userInfo.getOrg_name())
					.setDeleted(Status.FALSE.getKey())
					.setPayDate(rq.getPayDate());
			if (dinasPurchasePayMapper.insert(dinasPurchasePay)<0) {
				return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
			}
		}
		//更新采购合同中的已回款金额
		DinasPurchaseOrder purchaseOrder = dinasPurchaseOrderMapper.selectOne(new DinasPurchaseOrder()
				.setId(rq.getOrderId()).setDeleted(0));
		if (!ObjectUtils.isEmpty(purchaseOrder)) {
			BigDecimal payment = purchaseOrder.getPayment();
			BigDecimal availableAmount = purchaseOrder.getAvailableAmount();
			if (!ObjectUtils.isEmpty(rq.getAmount())) {
				payment = payment.add(rq.getAmount());
				availableAmount = availableAmount.add(rq.getAmount());
				purchaseOrder.setPayment(payment);
				purchaseOrder.setAvailableAmount(availableAmount);
				dinasPurchaseOrderMapper.updateById(purchaseOrder);
			}
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dinasPurchasePay.getId());
	}
	
	/**
	 * 查询采购付款列表
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<List<DinasPurchasePayDTO>> queryList(AuthPlatformUserInfo userInfo, DinasPurchasePayQueryRQ rq, Pagination pagination){
		rq.setCompanyId(userInfo.getOrgId());
		List<DinasPurchasePayDTO> list = dinasPurchasePayMapper.queryList(rq,pagination);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
	}
	
	/**
	 * 查看采购付款
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<DinasPurchasePayDTO> queryOne(AuthPlatformUserInfo userInfo,Integer id){
		DinasPurchasePay dinasPurchasePay = this.selectById(id).setDeleted(Status.FALSE.getKey());
		if (Objects.nonNull(dinasPurchasePay) && StringUtils.isNotBlank(dinasPurchasePay.getUrl())) {
			String[] urlList = dinasPurchasePay.getUrl().split(",");
			DinasPurchasePayDTO dto = BeanUtils.copyProperties(dinasPurchasePay, DinasPurchasePayDTO.class);
			DinasPurchaseOrder dinasPurchaseOrder = dinasPurchaseOrderMapper.selectOne(new DinasPurchaseOrder().setId(dinasPurchasePay.getOrderId()).setDeleted(Status.FALSE.getKey()));
			DinasCustomer dinasCustomer = dinasCustomerMapper.selectOne(new DinasCustomer().setId(dinasPurchasePay.getCustomerId()));
			if (Objects.nonNull(dinasCustomer) && Objects.nonNull(dinasPurchaseOrder)) {
				dto.setOrderCode(dinasPurchaseOrder.getCode())
					.setPayDate(dinasPurchasePay.getPayDate())
					.setCustomerName(dinasCustomer.getCustomerName())
					.setList(JSONObject.parseArray(dinasPurchasePay.getUrl(), DinasUrlDTO.class));
			}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
		}
		if (Objects.nonNull(dinasPurchasePay) && null == dinasPurchasePay.getUrl()) {
			DinasPurchasePayDTO dto = BeanUtils.copyProperties(dinasPurchasePay, DinasPurchasePayDTO.class);
			DinasPurchaseOrder dinasPurchaseOrder = dinasPurchaseOrderMapper.selectOne(new DinasPurchaseOrder().setId(dinasPurchasePay.getOrderId()).setDeleted(Status.FALSE.getKey()));
			DinasCustomer dinasCustomer = dinasCustomerMapper.selectOne(new DinasCustomer().setId(dinasPurchasePay.getCustomerId()));
			if (Objects.nonNull(dinasCustomer) && Objects.nonNull(dinasPurchaseOrder)) {
				dto.setOrderCode(dinasPurchaseOrder.getCode())
					.setPayDate(dinasPurchasePay.getPayDate())
					.setCustomerName(dinasCustomer.getCustomerName());
		}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
		}
		return null;
	}
	
	/**
	 * 修改采购发票
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> updatePurchasePay(AuthPlatformUserInfo userInfo, DinasPurchasePayRQ rq){
		if (null == rq.getId()) {
			return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_PURCHASE_PAY_NOT_ID);
		}
		DinasPurchasePay pay = dinasPurchasePayMapper.selectById(rq.getId()).setDeleted(Status.FALSE.getKey());
		BigDecimal paymentOri = paymentOri = pay.getAmount();
		//当传过来的URL不为空字符串的时候
		DinasPurchasePay dinasPurchasePay = BeanUtils.copyProperties(rq, pay);
		if (ObjectUtils.isEmpty(pay)){
			return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
		}
		dinasPurchasePay.setUpdateTime(new Date())
				.setUpdateUser(userInfo.getId())
				.setPayDate(rq.getPayDate());
		if (StringUtils.isNotBlank(pay.getUrl())){
			dinasPurchasePay.setUrl(appendixUtils.getJsonStr(rq.getList()));
		}
		if (dinasPurchasePayMapper.updateById(dinasPurchasePay)<1) {
			return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
		}
		//更新采购合同中的已回款金额
		DinasPurchaseOrder purchaseOrder = dinasPurchaseOrderMapper.selectOne(new DinasPurchaseOrder()
				.setId(rq.getOrderId()).setDeleted(0));
		if (!ObjectUtils.isEmpty(purchaseOrder)) {
			BigDecimal payment = purchaseOrder.getPayment();
			BigDecimal availableAmount = purchaseOrder.getAvailableAmount();
			BigDecimal newPayment = BigDecimal.ZERO;
			if (!ObjectUtils.isEmpty(rq.getAmount())) {
				newPayment = rq.getAmount();
			}
			payment = payment.add(newPayment.subtract(paymentOri));//.subtract(pay.getAmount()).add(newPayment);
			availableAmount = availableAmount.add(newPayment.subtract(paymentOri));//.subtract(pay.getAmount()).add(newPayment);
			purchaseOrder.setPayment(payment);
			purchaseOrder.setAvailableAmount(availableAmount);
			dinasPurchaseOrderMapper.updateById(purchaseOrder);
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dinasPurchasePay.getId());
		
	}
	
	/**
	 * 逻辑删除一条
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> deletePurchasePay(AuthPlatformUserInfo userInfo,Integer id) {
		Integer updateById = dinasPurchasePayMapper.updateById(new DinasPurchasePay().setDeleted(Status.TRUE.getKey()).setId(id).setUpdateUser(userInfo.getId()));
		log.info("修改采购付款=>updateById={}", updateById);
		 if (updateById < 0) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
	        }
		 return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
	}
	
	/**
	 * 批量删除
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> deleteBatch(AuthPlatformUserInfo userInfo,DinasPurchasePayDeleteBatchRQ rq) {
		List<Integer> list = rq.getList();
		if (list.size() > 0) {
			for (Integer id : list) {
				Integer updateById = dinasPurchasePayMapper.updateById(new DinasPurchasePay().setDeleted(Status.TRUE.getKey()).setId(id).setUpdateUser(userInfo.getId()));
				log.info("修改采购付款=>updateById={}", updateById);
				 if (updateById < 0) {
			            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
			        }
				 return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
			}
		}else {
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
	}
}

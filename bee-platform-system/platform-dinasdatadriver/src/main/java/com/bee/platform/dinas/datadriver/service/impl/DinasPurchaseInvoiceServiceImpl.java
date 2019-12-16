package com.bee.platform.dinas.datadriver.service.impl;

import com.bee.platform.dinas.datadriver.entity.DinasCustomer;
import com.bee.platform.dinas.datadriver.entity.DinasProduct;
import com.bee.platform.dinas.datadriver.entity.DinasProductSpec;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseInvoice;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrder;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceDeleteBatchRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasCustomerMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasProductMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasProductSpecMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasPurchaseInvoiceMapper;
import com.bee.platform.dinas.datadriver.dao.mapper.DinasPurchaseOrderMapper;
import com.bee.platform.dinas.datadriver.dto.DinasPurchaseInvoiceDTO;
import com.bee.platform.dinas.datadriver.dto.DinasUrlDTO;
import com.bee.platform.dinas.datadriver.service.DinasPurchaseInvoiceService;
import com.bee.platform.dinas.datadriver.utils.AppendixUtils;

import lombok.extern.slf4j.Slf4j;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * <p>
 * 采购发票 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
@Slf4j
@Service
public class DinasPurchaseInvoiceServiceImpl extends ServiceImpl<DinasPurchaseInvoiceMapper, DinasPurchaseInvoice> implements DinasPurchaseInvoiceService {

	@Autowired
	private DinasPurchaseInvoiceMapper dinasPurchaseInvoiceMapper;
	
	@Autowired
	private DinasPurchaseOrderMapper dinasPurchaseOrderMapper;
	
	@Autowired
	private DinasCustomerMapper dinasCustomerMapper;
	
	@Autowired
	private DinasProductMapper dinasProductMapper;
	
	@Autowired
	private DinasProductSpecMapper dinasProductSpecMapper;
	
	@Autowired
    private AppendixUtils appendixUtils;
	
	/**
	 * 新增采购发票
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> addInvoice(AuthPlatformUserInfo userInfo, DinasPurchaseInvoiceRQ rq) {
		DinasPurchaseInvoice dinasPurchaseInvoice = BeanUtils.copyProperties(rq, DinasPurchaseInvoice.class);
		SimpleDateFormat slf = new SimpleDateFormat("yyyy-MM-dd");
		if (CollectionUtils.isNotEmpty(rq.getList())) {
			try {
				dinasPurchaseInvoice.setCreateUser(userInfo.getId())
									.setCreateTime(new Date()).setCompanyId(userInfo.getOrgId()).setCompanyName(userInfo.getOrg_name())
									.setDeleted(Status.FALSE.getKey())
									.setInvoiceDate(slf.parse(rq.getInvoiceDate()))
									.setUrl(appendixUtils.getJsonStr(rq.getList()));
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (dinasPurchaseInvoiceMapper.insert(dinasPurchaseInvoice)<0) {
				return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
			}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dinasPurchaseInvoice.getId());
		}else {
			try {
				dinasPurchaseInvoice.setCreateUser(userInfo.getId())
									.setCreateTime(new Date()).setCompanyId(userInfo.getOrgId()).setCompanyName(userInfo.getOrg_name())
									.setDeleted(Status.FALSE.getKey())
									.setInvoiceDate(slf.parse(rq.getInvoiceDate()));
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (dinasPurchaseInvoiceMapper.insert(dinasPurchaseInvoice)<0) {
				return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
			}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dinasPurchaseInvoice.getId());
		}
		
	}
	
	
	/**
	 * 查询采购发票
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<List<DinasPurchaseInvoiceDTO>> queryList(AuthPlatformUserInfo userInfo,
			 DinasPurchaseInvoiceQueryRQ rq, Pagination pagination) {
		rq.setCompanyId(userInfo.getOrgId());
		List<DinasPurchaseInvoiceDTO> list = dinasPurchaseInvoiceMapper.queryList(rq,pagination);
		if (CollectionUtils.isEmpty(list)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
	}
	
	
	/**
	 * 查询一条采购发票
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<DinasPurchaseInvoiceDTO> queryOne(AuthPlatformUserInfo userInfo, Integer id) {
		DinasPurchaseInvoice dinasPurchaseInvoice = this.selectOne(new EntityWrapper<>(new DinasPurchaseInvoice().setId(id).setDeleted(Status.FALSE.getKey())));
		if (Objects.nonNull(dinasPurchaseInvoice) && StringUtils.isNotBlank(dinasPurchaseInvoice.getUrl())) {
			String[] urlList = dinasPurchaseInvoice.getUrl().split(",");
			DinasPurchaseInvoiceDTO dto = BeanUtils.copyProperties(dinasPurchaseInvoice, DinasPurchaseInvoiceDTO.class);
			DinasPurchaseOrder dinasPurchaseOrder = dinasPurchaseOrderMapper.selectOne(new DinasPurchaseOrder().setId(dinasPurchaseInvoice.getOrderId()).setDeleted(Status.FALSE.getKey()));
			DinasCustomer dinasCustomer = dinasCustomerMapper.selectOne(new DinasCustomer().setId(dinasPurchaseInvoice.getCustomerId()));
			DinasProduct dinasProduct = dinasProductMapper.selectOne(new DinasProduct().setId(dinasPurchaseInvoice.getProductId()));
			DinasProductSpec dinasProductSpec = dinasProductSpecMapper.selectOne(new DinasProductSpec().setId(dinasPurchaseInvoice.getProductSpecId()).setDeleted(Status.FALSE.getKey()));
			if (Objects.nonNull(dinasPurchaseOrder) && Objects.nonNull(dinasCustomer) && Objects.nonNull(dinasProduct) && Objects.nonNull(dinasProductSpec)) {
				dto.setOrderCode(dinasPurchaseOrder.getCode())
				.setCustomerName(dinasCustomer.getCustomerName())
				.setProductName(dinasProduct.getProductName())
				.setList(JSONObject.parseArray(dinasPurchaseInvoice.getUrl(), DinasUrlDTO.class))
				.setSpecName(dinasProductSpec.getSpecName());
			}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
		}
		if (Objects.nonNull(dinasPurchaseInvoice) && null == dinasPurchaseInvoice.getUrl()) {
			DinasPurchaseInvoiceDTO dto = BeanUtils.copyProperties(dinasPurchaseInvoice, DinasPurchaseInvoiceDTO.class);
			DinasPurchaseOrder dinasPurchaseOrder = dinasPurchaseOrderMapper.selectOne(new DinasPurchaseOrder().setId(dinasPurchaseInvoice.getOrderId()).setDeleted(Status.FALSE.getKey()));
			DinasCustomer dinasCustomer = dinasCustomerMapper.selectOne(new DinasCustomer().setId(dinasPurchaseInvoice.getCustomerId()));
			DinasProduct dinasProduct = dinasProductMapper.selectOne(new DinasProduct().setId(dinasPurchaseInvoice.getProductId()));
			DinasProductSpec dinasProductSpec = dinasProductSpecMapper.selectOne(new DinasProductSpec().setId(dinasPurchaseInvoice.getProductSpecId()).setDeleted(Status.FALSE.getKey()));
			if (Objects.nonNull(dinasPurchaseOrder) && Objects.nonNull(dinasCustomer) && Objects.nonNull(dinasProduct) && Objects.nonNull(dinasProductSpec)) {
				dto.setOrderCode(dinasPurchaseOrder.getCode())
				.setCustomerName(dinasCustomer.getCustomerName())
				.setProductName(dinasProduct.getProductName())
				.setSpecName(dinasProductSpec.getSpecName());
			}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
		}
		return null;
	}
	
	
	/**
	 * 编辑采购发票
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> updatePurchaseInvoice(AuthPlatformUserInfo userInfo, DinasPurchaseInvoiceRQ rq) {
		if (null == rq.getId()) {
			return ResponseResult.buildResponseResult(ResCodeEnum.DINAS_PURCHASE_INVOICE_NOT_ID);
		}
		DinasPurchaseInvoice dinasPurchaseInvoice = this.selectOne(new EntityWrapper<>(new DinasPurchaseInvoice().setId(rq.getId()).setDeleted(Status.FALSE.getKey())));
		//当传过来的URL不为空字符串的时候
		//if (Objects.nonNull(dinasPurchaseInvoice) && StringUtils.isNotBlank(dinasPurchaseInvoice.getUrl())) {
		if (CollectionUtils.isNotEmpty(rq.getList())){
			DinasPurchaseInvoice invoice = BeanUtils.copyProperties(rq, DinasPurchaseInvoice.class);
			SimpleDateFormat slf = new SimpleDateFormat("yyyy-MM-dd");
			try {
				invoice.setUpdateUser(userInfo.getId())
				       .setUpdateTime(new Date())
				       .setInvoiceDate(slf.parse(rq.getInvoiceDate()))
				       .setUrl(appendixUtils.getJsonStr(rq.getList()));
			} catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if (dinasPurchaseInvoiceMapper.updateById(invoice)<1) {
				return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
			}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, invoice.getId());
		}else {
		//当传过来的URL为空字符串的时候
		/*if (CollectionUtils.isNotEmpty(rq.getList())) {*/
			DinasPurchaseInvoice invoice = BeanUtils.copyProperties(rq, DinasPurchaseInvoice.class);
			SimpleDateFormat slf = new SimpleDateFormat("yyyy-MM-dd");
			try {
				invoice.setUpdateUser(userInfo.getId())
				       .setUpdateTime(new Date())
				       .setInvoiceDate(slf.parse(rq.getInvoiceDate()));
			} catch (ParseException e) {
				e.printStackTrace();
			}
			if (dinasPurchaseInvoiceMapper.updateById(invoice)<1) {
				return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
			}
			return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, invoice.getId());
		}
	}
	
	
	/**
	 * 逻辑删除一条采购发票
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> deletePurchaseInvoice(AuthPlatformUserInfo userInfo,Integer id) {
		Integer updateById = dinasPurchaseInvoiceMapper.updateById(new DinasPurchaseInvoice().setId(id).setDeleted(Status.TRUE.getKey()).setUpdateUser(userInfo.getId()).setUpdateTime(new Date()));
		log.info("修改采购发票=>updateById={}", updateById);
		if (updateById < 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
	 return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
	}
	
	
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> deleteBatch(AuthPlatformUserInfo userInfo, DinasPurchaseInvoiceDeleteBatchRQ rq) {
		List<Integer> list = rq.getList();
		if (list.size() > 0) {
			for (Integer id : list) {
				Integer updateById = dinasPurchaseInvoiceMapper.updateById(new DinasPurchaseInvoice().setId(id)
						.setDeleted(Status.TRUE.getKey()).setUpdateUser(userInfo.getId()).setUpdateTime(new Date()));
				log.info("修改采购发票=>updateById={}", updateById);
				if (updateById < 0) {
					return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
				}
			}
		}else {
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
	}
}

package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.ErpLogisticsInvoiceDetailMapper;
import com.bee.platform.datadriver.dao.mapper.ErpLogisticsInvoiceMapper;
import com.bee.platform.datadriver.entity.ErpLogisticsInvoice;
import com.bee.platform.datadriver.entity.ErpLogisticsInvoiceDetail;
import com.bee.platform.datadriver.rq.ErpLogisticsInvoiceDetailRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsInvoiceRQ;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.datadriver.service.ErpLogisticsInvoiceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 物流发票 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Slf4j
@Service
public class ErpLogisticsInvoiceServiceImpl extends ServiceImpl<ErpLogisticsInvoiceMapper, ErpLogisticsInvoice>
		implements ErpLogisticsInvoiceService {

	@Autowired
	private CommonService commonService;

	@Autowired
	private JedisService jedisService;

	@Autowired
	private ErpLogisticsInvoiceMapper erpLogisticsInvoiceMapper;

	@Autowired
	private ErpLogisticsInvoiceDetailMapper erpLogisticsInvoiceDetailMapper;

	/**
	 * 添加物流发票
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult<?> add(AuthPlatformUserInfo userInfo, ErpLogisticsInvoiceRQ rq) {
		List<ErpLogisticsInvoiceDetail> erpLogisticsinvoiceDetailList = new ArrayList<>();
		List<ErpLogisticsInvoiceDetailRQ> list = rq.getList();
		ErpLogisticsInvoice invoice = erpLogisticsInvoiceMapper
				.selectOne(new ErpLogisticsInvoice().setOrderId(rq.getOrderId()));
		if (null != invoice) {
			return ResponseResult.buildResponseResult(ResCodeEnum.ORDER_ID_NAME_REPEAT);
		}
		erpLogisticsinvoiceDetailList = BeanUtils.assemble(ErpLogisticsInvoiceDetail.class, list);
		// 新增
		ErpLogisticsInvoice erpLogisticsInvoice = BeanUtils.copyProperties(rq, ErpLogisticsInvoice.class);
		// 自动生产物流发票号
		// 编码
		String redisKey = "SIPFP" + userInfo.getOrgId();
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyyMMdd");
		String format = simpleDateFormat.format(new Date());
		int invoiceCode = jedisService.incrOne(redisKey);
		erpLogisticsInvoice.setInvoiceNumber("PFP" + format + commonService.getCode(invoiceCode + "", 3));

		erpLogisticsInvoice.setCreateUser(userInfo.getId()).setCreateTime(new Date());
		erpLogisticsInvoiceMapper.insert(erpLogisticsInvoice);
		// 添加物流发票详情
		erpLogisticsinvoiceDetailList.forEach(
				a -> a.setCreateTime(new Date()).setCreateUser(userInfo.getId()).setDeleted(Status.FALSE.getKey())
						.setOrderId(rq.getOrderId()).setInvoiceId(erpLogisticsInvoice.getId()));
		for (ErpLogisticsInvoiceDetail erpLogisticsInvoiceDetail : erpLogisticsinvoiceDetailList) {
			if (erpLogisticsInvoiceDetailMapper.insert(erpLogisticsInvoiceDetail) < 0) {
				log.error("新增物流状态详情失败，类：{} 方法：{}", "ErpLogisticsInvoiceDetailServiceImpl");
			}
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,erpLogisticsInvoice.getId());

	}

	@Override
	public ResponseResult<?> update(AuthPlatformUserInfo userInfo, ErpLogisticsInvoiceRQ rq) {
		List<ErpLogisticsInvoiceDetailRQ> list = rq.getList();
		// 修改
		if (null != rq.getId()) {
			ErpLogisticsInvoice logisticsInvoice = this.selectOne(new EntityWrapper<>(new ErpLogisticsInvoice()
					.setOrderId(rq.getOrderId()).setInvoiceNumber(rq.getInvoiceNumber())));
			
			if (Objects.nonNull(logisticsInvoice) && (rq.getId()).equals(logisticsInvoice.getId())) {
				
				ErpLogisticsInvoice erpLogisticsInvoice = BeanUtils.copyProperties(rq, ErpLogisticsInvoice.class);
				erpLogisticsInvoice.setUpdateTime(new Date()).setUpdateUser(userInfo.getId());
				Integer updateByIdRes = erpLogisticsInvoiceMapper.updateById(erpLogisticsInvoice);
				log.info("修改物流发票=>updateByIdRes={}", updateByIdRes);
				for (ErpLogisticsInvoiceDetailRQ erpLogisticsInvoiceDetailRQ : list) {
					Integer id = erpLogisticsInvoiceDetailRQ.getId();
					if (null != id) {
						ErpLogisticsInvoiceDetail erpLogisticsInvoiceDetail = BeanUtils
								.copyProperties(erpLogisticsInvoiceDetailRQ, ErpLogisticsInvoiceDetail.class);
						erpLogisticsInvoiceDetail.setUpdateTime(new Date()).setUpdateUser(userInfo.getId());
						erpLogisticsInvoiceDetailMapper.updateById(erpLogisticsInvoiceDetail);
					}else {
						ErpLogisticsInvoiceDetail erpLogisticsInvoiceDetail = BeanUtils
								.copyProperties(erpLogisticsInvoiceDetailRQ, ErpLogisticsInvoiceDetail.class);
						erpLogisticsInvoiceDetail.setOrderId(rq.getOrderId()).setCreateTime(new Date()).setCreateUser(userInfo.getId());
						erpLogisticsInvoiceDetailMapper.insert(erpLogisticsInvoiceDetail);
					}
				}
			}else if (Objects.isNull(logisticsInvoice)) {
				ErpLogisticsInvoice erpLogisticsInvoice = BeanUtils.copyProperties(rq, ErpLogisticsInvoice.class);
				erpLogisticsInvoice.setUpdateTime(new Date()).setUpdateUser(userInfo.getId());
				Integer updateByIdRes = erpLogisticsInvoiceMapper.updateById(erpLogisticsInvoice);
				log.info("修改物流发票=>updateByIdRes={}", updateByIdRes);
				for (ErpLogisticsInvoiceDetailRQ erpLogisticsInvoiceDetailRQ : list) {
					Integer id = erpLogisticsInvoiceDetailRQ.getId();
					if (null != id) {
						ErpLogisticsInvoiceDetail erpLogisticsInvoiceDetail = BeanUtils
								.copyProperties(erpLogisticsInvoiceDetailRQ, ErpLogisticsInvoiceDetail.class);
						erpLogisticsInvoiceDetail.setUpdateTime(new Date()).setUpdateUser(userInfo.getId());
						erpLogisticsInvoiceDetailMapper.updateById(erpLogisticsInvoiceDetail);
					}else {
						ErpLogisticsInvoiceDetail erpLogisticsInvoiceDetail = BeanUtils
								.copyProperties(erpLogisticsInvoiceDetailRQ, ErpLogisticsInvoiceDetail.class);
						erpLogisticsInvoiceDetail.setOrderId(rq.getOrderId()).setCreateTime(new Date()).setCreateUser(userInfo.getId());
						erpLogisticsInvoiceDetailMapper.insert(erpLogisticsInvoiceDetail);
					}
				}
			}else if (Objects.nonNull(logisticsInvoice) && !(rq.getId()).equals(logisticsInvoice.getId())) {
				return ResponseResult.buildResponseResult(ResCodeEnum.INVOICE_NUMBER_NOT);
			}
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS ,rq.getId());
	}

	@Override
	public ResponseResult<?> delete(AuthPlatformUserInfo userInfo, Integer id) {
		Integer updateInvoiceDetailRes= erpLogisticsInvoiceDetailMapper.updateById(new ErpLogisticsInvoiceDetail()
				.setId(id)
				.setDeleted(Status.TRUE.getKey()));
		log.info("修改物流发票的明细=>updateInvoiceDetailRes={}", updateInvoiceDetailRes);
		if (updateInvoiceDetailRes < 0) {
			return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
	}
}

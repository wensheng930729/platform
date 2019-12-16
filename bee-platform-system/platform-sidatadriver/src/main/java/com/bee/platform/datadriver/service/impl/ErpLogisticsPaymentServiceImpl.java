package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.ErpLogisticsOrdersMapper;
import com.bee.platform.datadriver.dao.mapper.ErpLogisticsPaymentMapper;
import com.bee.platform.datadriver.entity.ErpLogisticsOrders;
import com.bee.platform.datadriver.entity.ErpLogisticsPayment;
import com.bee.platform.datadriver.rq.ErpLogisticsPaymentRQ;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.datadriver.service.ErpLogisticsPaymentService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Objects;

/**
 * <p>
 * 物流付款 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Slf4j
@Service
public class ErpLogisticsPaymentServiceImpl extends ServiceImpl<ErpLogisticsPaymentMapper, ErpLogisticsPayment> implements ErpLogisticsPaymentService {

	@Autowired
	private CommonService commonService;
	
	@Autowired
	private JedisService jedisService;
	
	@Autowired
	private  ErpLogisticsPaymentMapper erpLogisticsPaymentMapper;

	@Autowired
	private ErpLogisticsOrdersMapper logisticsOrdersMapper;
	
	  /**
     *  添加物流付款
     */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult add(AuthPlatformUserInfo userInfo, ErpLogisticsPaymentRQ rq) {

		ErpLogisticsPayment erpLogisticsPayment = BeanUtils.copyProperties(rq, ErpLogisticsPayment.class);
		if (StringUtils.isBlank(rq.getPayOrderNo())) {
			String redisKey = "SIFK" + userInfo.getOrgId();
			SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyyMMdd");
			String format = simpleDateFormat.format(new Date());
			int payCode = jedisService.incrOne(redisKey);
			erpLogisticsPayment.setPayOrderNo("FK" + format + commonService.getCode(payCode + "", 3));
//			// redis序列自增
//			jedisService.incrOne(redisKey);
		}
		erpLogisticsPayment.setCreateUser(userInfo.getId()).setCreateTime(new Date()).setDeleted(Status.FALSE.getKey());
		if (erpLogisticsPaymentMapper.insert(erpLogisticsPayment) < 0) {
			return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
		}
		// 修改订单付款状态
		if (logisticsOrdersMapper.updateById(new ErpLogisticsOrders().setId(rq.getOrderId()).setPayStatus(1)
				.setUpdateUser(userInfo.getId()).setUpdateTime(new Date())) < 0){
			throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.WL_UPDATE_PAY_STATUS_FAILED);
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,erpLogisticsPayment.getId());
		
	}

	/**
	 * 编辑物流付款
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
	public ResponseResult updatePay(AuthPlatformUserInfo userInfo, ErpLogisticsPaymentRQ rq) {
		Integer id = rq.getId();
		if (Objects.nonNull(id)) {
			ErpLogisticsPayment selectOne = this.selectOne(new EntityWrapper<>(new ErpLogisticsPayment()
					.setOrderId(rq.getOrderId()).setPayOrderNo(rq.getPayOrderNo()).setDeleted(Status.FALSE.getKey())));
			
			if (Objects.nonNull(selectOne) && id .equals (selectOne.getId())) {
				ErpLogisticsPayment erpLogisticsPayment = BeanUtils.copyProperties(rq, ErpLogisticsPayment.class);
				erpLogisticsPayment.setId(rq.getId()).setUpdateTime(new Date()).setUpdateUser(userInfo.getId());
				if (erpLogisticsPaymentMapper.updateById(erpLogisticsPayment)<1) {
					return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
				}
			}else if (Objects.isNull(selectOne)) {
				ErpLogisticsPayment erpLogisticsPayment = BeanUtils.copyProperties(rq, ErpLogisticsPayment.class);
				erpLogisticsPayment.setId(rq.getId()).setUpdateTime(new Date()).setUpdateUser(userInfo.getId());
				if (erpLogisticsPaymentMapper.updateById(erpLogisticsPayment)<1) {
					return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
				}
			}else if (Objects.nonNull(selectOne) && !id .equals (selectOne.getId())) {
				return ResponseResult.buildResponseResult(ResCodeEnum.PAY_ORDER_NO_NOT);
			}
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);

	}
	
	
	 /**
     * 删除物流付款单
     */
    @Override
    public ResponseResult<?> deletePayment(AuthPlatformUserInfo userInfo, Integer id) {
        Integer updateById = erpLogisticsPaymentMapper.updateById(new ErpLogisticsPayment().setId(id).setDeleted(Status.TRUE.getKey()));
        log.info("修改物流订单的明细=>updateById={}", updateById);
        if (updateById < 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }
}

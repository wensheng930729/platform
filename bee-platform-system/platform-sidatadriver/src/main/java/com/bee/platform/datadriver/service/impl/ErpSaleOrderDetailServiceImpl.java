package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.datadriver.entity.ErpSaleOrder;
import com.bee.platform.datadriver.entity.ErpSaleOrderDetail;
import com.bee.platform.datadriver.rq.ErpSaleOrderDetailAddRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.customer.dto.ErpSaleOrderDetailDTO;
import com.bee.platform.datadriver.dao.mapper.ErpSaleOrderDetailMapper;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDetailDTO;
import com.bee.platform.datadriver.service.ErpSaleOrderDetailService;

import com.bee.platform.datadriver.service.ErpSaleOrderService;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import javax.validation.constraints.NotNull;

/**
 * <p>
 * 销售单明细 服务实现类
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpSaleOrderDetailServiceImpl extends ServiceImpl<ErpSaleOrderDetailMapper, ErpSaleOrderDetail> implements ErpSaleOrderDetailService {
	@Autowired
	private ErpSaleOrderDetailMapper erpSaleOrderDetailMapper;
	@Autowired
	private ErpSaleOrderService saleOrderService;
	private static Integer ZERO = 0;
	
	
	/**
	 * 查看销售订单详情
	 */
	@Override
    public ResponseResult<List<ErpSaleOrderDetailDTO>> getErpSaleOrderDetail(AuthPlatformUserInfo userInfo,int orderId) {
		List<ErpSaleOrderDetailDTO> list = erpSaleOrderDetailMapper.selectErpSaleOrderDetail(orderId);
		if (CollectionUtils.isNotEmpty(list)) {
			list.forEach(sod -> {
				if (StringUtils.isNotBlank(sod.getProductName()) && StringUtils.isNotBlank(sod.getBatchName())) {
					sod.setProductName(sod.getProductName() + "/" + sod.getBatchName());
				}
			});
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
	}
	
	
	/**
	 * 删除订单详情
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> deleteErpSaleOrderDetail(AuthPlatformUserInfo userInfo, int id) {
		ErpSaleOrderDetail saleOrderDetail = selectOne(new EntityWrapper<>(new ErpSaleOrderDetail()
				.setId(id).setDeleted(Status.FALSE.getKey())));
		if (Objects.isNull(saleOrderDetail)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
		}
		if (erpSaleOrderDetailMapper.updateById(new ErpSaleOrderDetail()
                .setId(id).setDeleted(Status.TRUE.getKey())) <= ZERO){
            log.error("删除销售单明细失败,调用{}类{}方法出错","ErpSaleOrderDetailServiceImpl","deleteErpSaleOrderDetail()");
            throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
        }
		Integer orderId = saleOrderDetail.getOrderId();
		ErpSaleOrder erpSaleOrder = saleOrderService.selectById(orderId);
		if(!ObjectUtils.isEmpty(erpSaleOrder)){
			BigDecimal existAmount = erpSaleOrder.getAmount();
			if (Objects.isNull(existAmount)) {
				existAmount = BigDecimal.ZERO;
			}
			BigDecimal detailAmount = saleOrderDetail.getAmount();
			if (Objects.isNull(detailAmount)) {
			    detailAmount = BigDecimal.ZERO;
            }
			erpSaleOrder.setAmount(existAmount.subtract(detailAmount));
			if(!saleOrderService.updateById(erpSaleOrder)){
				log.error("删除销售单明细失败,调用{}类{}方法出错","ErpSaleOrderDetailServiceImpl","deleteErpSaleOrderDetail()");
				throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
			}
		}

		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleOrderDetail.getOrderId());
	}
	
	/**
	 * 修改销售订单详情
	 */
	@Override
	@Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateErpSaleOrderDetail(AuthPlatformUserInfo userInfo,ErpSaleOrderDetailAddRQ rq) {
		ErpSaleOrderDetail erpSaleOrderDetail = BeanUtils.copyProperties(rq, ErpSaleOrderDetail.class);
		Integer pOrderId = rq.getOrderId();
		BigDecimal dAmount = rq.getAmount();
		ErpSaleOrder erpSaleOrder = saleOrderService.selectById(pOrderId);
		//判断当前订单下的产品是否唯一
		if (Objects.nonNull(erpSaleOrder)) {
            List<ErpSaleOrderDetail> erpSaleOrderDetails = this.selectList(new EntityWrapper<>(new ErpSaleOrderDetail()
                    .setOrderId(erpSaleOrder.getId())
                    .setDeleted(Status.FALSE.getKey())));
            if (CollectionUtils.isNotEmpty(erpSaleOrderDetails)) {
                 Integer productId = erpSaleOrderDetails.get(0).getProductId();
                 if (Objects.nonNull(productId) && (Objects.nonNull(rq.getProductId()) && !productId.equals(rq.getProductId()))) {
                     return ResponseResult.buildResponseResult(ResCodeEnum.PRODUCT_NOT_UNIQUE);
                 }
            }
        }

		if (Objects.isNull(rq.getId())) {
			if(!ObjectUtils.isEmpty(dAmount)){
				if (Objects.nonNull(erpSaleOrder)){
					BigDecimal existAmount = erpSaleOrder.getAmount();
					if (Objects.nonNull(existAmount)) {
                        erpSaleOrder.setAmount(existAmount.add(dAmount));
                    }
					if(!saleOrderService.updateById(erpSaleOrder)){
						log.error("新增销售单明细失败,调用{}类{}方法出错","ErpSaleOrderDetailServiceImpl","updateErpSaleOrderDetail()");
						throw new BusinessException(ResCodeEnum.ADD_FAILED, ExceptionMessageEnum.ERROR_SYSTEM);
					}
				}
			}
			//新增销售订单详情
			if (erpSaleOrderDetailMapper.insert(erpSaleOrderDetail
					.setCreateUser(userInfo.getId())
					.setCreateTime(new Date())) <= ZERO){
				log.error("新增销售单明细失败,调用{}类{}方法出错","ErpSaleOrderDetailServiceImpl","updateErpSaleOrderDetail()");
				throw new BusinessException(ResCodeEnum.ADD_FAILED, ExceptionMessageEnum.ERROR_SYSTEM);
			}


		} else {
			ErpSaleOrderDetail oldDetail = selectById(rq.getId());
			if(ObjectUtils.isEmpty(oldDetail)){
				return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA, rq.getOrderId());
			}

			if(!ObjectUtils.isEmpty(dAmount)){
				if (Objects.nonNull(erpSaleOrder)){
					BigDecimal existAmount = erpSaleOrder.getAmount();
					if (Objects.nonNull(existAmount)) {
                        BigDecimal detailAmount = oldDetail.getAmount();
					    if (Objects.isNull(detailAmount)) {
					        detailAmount = BigDecimal.ZERO;
                        }
                        erpSaleOrder.setAmount(existAmount.add(dAmount).subtract(detailAmount));
                    }
					if(!saleOrderService.updateById(erpSaleOrder)){
						log.error("更新销售单明细失败,调用{}类{}方法出错","ErpSaleOrderDetailServiceImpl","updateErpSaleOrderDetail()");
						throw new BusinessException(ResCodeEnum.UPDATA_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
					}
				}
			}


			if (erpSaleOrderDetailMapper.updateById(erpSaleOrderDetail
					.setId(rq.getId())
					.setCreateUser(userInfo.getId())
					.setCreateTime(new Date())) <= ZERO){
				log.error("新增销售单明细失败,调用{}类{}方法出错","ErpSaleOrderDetailServiceImpl","addErpErpSaleOrderDetail()");
				throw new BusinessException(ResCodeEnum.DELETE_FAIL, ExceptionMessageEnum.ERROR_SYSTEM);
			}
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, rq.getOrderId());
	}


		
}

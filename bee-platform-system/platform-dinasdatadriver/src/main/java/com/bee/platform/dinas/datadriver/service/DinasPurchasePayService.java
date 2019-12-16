package com.bee.platform.dinas.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.DinasPurchasePayDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchasePay;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayDeleteBatchRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayRQ;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 采购付款单 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
public interface DinasPurchasePayService extends IService<DinasPurchasePay> {

	ResponseResult<?> addPurchasePay(AuthPlatformUserInfo userInfo, DinasPurchasePayRQ rq);

	ResponseResult<List<DinasPurchasePayDTO>> queryList(AuthPlatformUserInfo userInfo, DinasPurchasePayQueryRQ rq,
			Pagination pagination);

	ResponseResult<DinasPurchasePayDTO> queryOne(AuthPlatformUserInfo userInfo, Integer id);

	ResponseResult<?> updatePurchasePay(AuthPlatformUserInfo userInfo, DinasPurchasePayRQ rq);

	ResponseResult<?> deletePurchasePay(AuthPlatformUserInfo userInfo, Integer id);

	ResponseResult<?> deleteBatch(AuthPlatformUserInfo userInfo, DinasPurchasePayDeleteBatchRQ rq);

}

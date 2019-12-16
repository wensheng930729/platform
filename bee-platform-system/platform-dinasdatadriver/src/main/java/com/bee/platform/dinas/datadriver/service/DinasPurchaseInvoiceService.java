package com.bee.platform.dinas.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.DinasPurchaseInvoiceDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseInvoice;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceDeleteBatchRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceRQ;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 采购发票 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
public interface DinasPurchaseInvoiceService extends IService<DinasPurchaseInvoice> {

	ResponseResult<?> addInvoice(AuthPlatformUserInfo userInfo, DinasPurchaseInvoiceRQ rq);

	ResponseResult<List<DinasPurchaseInvoiceDTO>> queryList(AuthPlatformUserInfo userInfo,
			DinasPurchaseInvoiceQueryRQ rq, Pagination pagination);

	ResponseResult<DinasPurchaseInvoiceDTO> queryOne(AuthPlatformUserInfo userInfo, Integer id);

	ResponseResult<?> updatePurchaseInvoice(AuthPlatformUserInfo userInfo, DinasPurchaseInvoiceRQ rq);

	ResponseResult<?> deletePurchaseInvoice(AuthPlatformUserInfo userInfo, Integer id);

	ResponseResult<?> deleteBatch(AuthPlatformUserInfo userInfo, DinasPurchaseInvoiceDeleteBatchRQ rq);

}

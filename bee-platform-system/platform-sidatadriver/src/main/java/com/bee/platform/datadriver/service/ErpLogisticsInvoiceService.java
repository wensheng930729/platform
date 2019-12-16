package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.entity.ErpLogisticsInvoice;
import com.bee.platform.datadriver.rq.ErpLogisticsInvoiceRQ;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 物流发票 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
public interface ErpLogisticsInvoiceService extends IService<ErpLogisticsInvoice> {

	/**
	 * 物流发票添加和修改
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<?> add(AuthPlatformUserInfo userInfo, ErpLogisticsInvoiceRQ rq);
	
	/**
	 * 物流发票添加和修改
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<?> update(AuthPlatformUserInfo userInfo, ErpLogisticsInvoiceRQ rq);
	
	/**
	 * 物流发票删除明细
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<?> delete(AuthPlatformUserInfo userInfo, Integer id);


}

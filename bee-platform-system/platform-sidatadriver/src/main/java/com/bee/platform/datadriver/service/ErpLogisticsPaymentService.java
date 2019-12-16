package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.entity.ErpLogisticsPayment;
import com.bee.platform.datadriver.rq.ErpLogisticsPaymentRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsPaymentRemarksRQ;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 物流付款 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
public interface ErpLogisticsPaymentService extends IService<ErpLogisticsPayment> {

	ResponseResult updatePay(AuthPlatformUserInfo userInfo, ErpLogisticsPaymentRQ rq);

	ResponseResult add(AuthPlatformUserInfo userInfo, ErpLogisticsPaymentRQ rq);

	ResponseResult<?> deletePayment(AuthPlatformUserInfo userInfo, Integer id);

}

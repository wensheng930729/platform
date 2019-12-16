package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.entity.ErpLogisticsOrdersDetail;
import com.bee.platform.datadriver.rq.ErpLogisticsOrdersDetailAddRQ;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 物流订单明细 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
public interface ErpLogisticsOrdersDetailService extends IService<ErpLogisticsOrdersDetail> {

	ResponseResult add(AuthPlatformUserInfo userInfo, ErpLogisticsOrdersDetailAddRQ rq);

}

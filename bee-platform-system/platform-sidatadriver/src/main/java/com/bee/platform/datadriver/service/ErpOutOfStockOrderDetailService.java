package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpOutOfStockOrderDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpOutOfStockOrderDetailRQ;

/**
 * <p>
 * 领料出库明细表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpOutOfStockOrderDetailService extends IService<ErpOutOfStockOrderDetail> {

    Integer saveOutOfStockOrderDetail(AuthPlatformUserInfo userInfo, ErpOutOfStockOrderDetailRQ rq);

    void deleteOutOfStockOrderDetailById(AuthPlatformUserInfo userInfo, Integer id);

    ErpOutOfStockOrderDetailDTO getOutOfStockOrderDetailById(Integer id);
}

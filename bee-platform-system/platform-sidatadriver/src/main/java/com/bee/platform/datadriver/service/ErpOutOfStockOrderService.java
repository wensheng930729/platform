package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderDTO;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderSearchListDTO;
import com.bee.platform.datadriver.entity.ErpOutOfStockOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpOutOfStockOrderRQ;
import com.bee.platform.datadriver.rq.ErpOutOfStockSearchRQ;

import java.util.List;

/**
 * <p>
 * 领料出库主表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpOutOfStockOrderService extends IService<ErpOutOfStockOrder> {

    ResponseResult<List<ErpOutOfStockOrderSearchListDTO>> searchOutOfStockOrderByCondition( ErpOutOfStockSearchRQ rq, Page page, Integer companyId);

    void deleteOutOfStockOrderById(AuthPlatformUserInfo userInfo, Integer id);

    void updateOutOfStockOrderState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    Integer saveOutOfStockOrder(AuthPlatformUserInfo userInfo, ErpOutOfStockOrderRQ rq);

    ErpOutOfStockOrderDTO getOutOfStockOrderById(Integer id);
}

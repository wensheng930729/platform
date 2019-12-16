package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpWarehousingOrder;
import com.bee.platform.datadriver.rq.ErpGetOneWarehousingOrderRQ;
import com.bee.platform.datadriver.rq.ErpWarehousingOrderRQ;
import com.bee.platform.datadriver.rq.ErpWarehousingOrderSearchRQ;

import java.util.List;

/**
 * <p>
 * 成品入库主表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpWarehousingOrderService extends IService<ErpWarehousingOrder> {

    ResponseResult<List<ErpWarehousingOrderSearchListDTO>> searchWarehousingOrderByCondition(ErpWarehousingOrderSearchRQ rq, Page page, Integer companyId);

    ErpWarehousingOrderDTO getWarehousingOrderById(Integer id);

    void deleteWarehousingOrderById(AuthPlatformUserInfo userInfo, Integer id);

    void updateWarehousingOrderState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    Integer saveWarehousingOrder(AuthPlatformUserInfo userInfo, ErpWarehousingOrderRQ rq);

    List<ErpWarehousingListDTO> getUserWarehousingOrderList(Integer companyId);

    ErpMaterialBatchProductDTO getProductByMaterialBatchId(Integer id);

    ErpGetOneWarehousingDTO getOneWarehousingOrder(ErpGetOneWarehousingOrderRQ rq);

    ProductAndOtherDTO getProductAndOtherByTestReportId(Integer id);
}

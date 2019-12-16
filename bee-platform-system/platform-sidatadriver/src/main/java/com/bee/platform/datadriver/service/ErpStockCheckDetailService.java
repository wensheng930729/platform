package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpStockCheckDetailDTO;
import com.bee.platform.datadriver.entity.ErpStockCheckDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpStockCheckDetailRQ;

/**
 * <p>
 * 库存盘点明细 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpStockCheckDetailService extends IService<ErpStockCheckDetail> {

    Integer saveStockCheckDetail(AuthPlatformUserInfo userInfo, ErpStockCheckDetailRQ rq);

    void deleteStockCheckDetailById(AuthPlatformUserInfo userInfo, Integer id);

    ErpStockCheckDetailDTO getStockCheckDetailById(Integer id);
}

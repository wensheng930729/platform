package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpStockSearchListDTO;
import com.bee.platform.datadriver.entity.ErpStock;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpStockSearchRQ;

import java.util.List;

/**
 * <p>
 * 库存表 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-03
 */
public interface ErpStockService extends IService<ErpStock> {

    ResponseResult<List<ErpStockSearchListDTO>> searchStockByCondition(ErpStockSearchRQ rq, Page page, Integer companyId);
}

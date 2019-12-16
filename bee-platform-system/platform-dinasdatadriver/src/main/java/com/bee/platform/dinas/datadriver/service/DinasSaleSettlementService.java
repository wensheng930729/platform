package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.DinasSaleSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleSettlement;
import com.bee.platform.dinas.datadriver.rq.DinasSaleSettlementSearchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementBatchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementRQ;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 销售结算表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasSaleSettlementService extends IService<DinasSaleSettlement> {

    ResponseResult<List<DinasSaleSettlementSearchDTO>> searchSaleSettlementByCondition(DinasSaleSettlementSearchRQ rq, Page page, Integer companyId);

    List<Integer> cancelSettlementByIds(AuthPlatformUserInfo userInfo, List<Integer> ids);

    Integer settlementOne(AuthPlatformUserInfo userInfo, SettlementRQ rq);

    List<Integer> settlementBatch(AuthPlatformUserInfo userInfo, SettlementBatchRQ rq);

    BigDecimal getAvailableAmount(Integer contractId);
}

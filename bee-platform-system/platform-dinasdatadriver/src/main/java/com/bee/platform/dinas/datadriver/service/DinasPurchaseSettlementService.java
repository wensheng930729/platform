package com.bee.platform.dinas.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.DinasPurchaseSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseSettlement;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.dinas.datadriver.rq.SettlementBatchRQ;
import com.bee.platform.dinas.datadriver.rq.SettlementRQ;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseSettlementSearchRQ;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 采购结算表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasPurchaseSettlementService extends IService<DinasPurchaseSettlement> {

    ResponseResult<List<DinasPurchaseSettlementSearchDTO>> searchPurchaseSettlementByCondition(DinasPurchaseSettlementSearchRQ rq, Page page, Integer companyId);

    List<Integer> cancelSettlementByIds(AuthPlatformUserInfo userInfo, List<Integer> ids);

    Integer settlementOne(AuthPlatformUserInfo userInfo, SettlementRQ rq);

    List<Integer> settlementBatch(AuthPlatformUserInfo userInfo, SettlementBatchRQ rq);

    BigDecimal getAvailableAmount(Integer contractId);
}

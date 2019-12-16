package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.DinasPurchaseSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseSettlement;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseSettlementSearchRQ;

import java.util.List;

/**
 * <p>
 * 采购结算表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasPurchaseSettlementMapper extends BaseMapper<DinasPurchaseSettlement> {

    List<DinasPurchaseSettlementSearchDTO> searchPurchaseSettlementByCondition(DinasPurchaseSettlementSearchRQ rq, Pagination pagination);
}

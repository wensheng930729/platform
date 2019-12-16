package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.DinasSaleSettlementSearchDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleSettlement;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.rq.DinasSaleSettlementSearchRQ;

import java.util.List;

/**
 * <p>
 * 销售结算表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasSaleSettlementMapper extends BaseMapper<DinasSaleSettlement> {

    List<DinasSaleSettlementSearchDTO> searchSaleSettlementByCondition(DinasSaleSettlementSearchRQ rq, Pagination pagination);
}

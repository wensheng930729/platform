package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpPurchaseStatementDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseStatement;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpPurchaseStatementRQ;

import java.util.List;

/**
 * <p>
 * 采购结算单 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseStatementMapper extends BaseMapper<ErpPurchaseStatement> {

    int batchDelete(List<Integer> list);

    List<ErpPurchaseStatementDetailDTO> findStatementList(ErpPurchaseStatementRQ erpPurchaseStatementRQ, Pagination pagination);

}

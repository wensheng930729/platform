package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpSaleStatementDetailDTO;
import com.bee.platform.datadriver.entity.ErpSaleStmtDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpSaleStatementSelectRQ;

import java.util.List;

/**
 * <p>
 * 销售结算单结算详情 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpSaleStmtDetailMapper extends BaseMapper<ErpSaleStmtDetail> {

    int batchDeleteStatementDetailByStmtIds(List<Integer> list);

    List<ErpSaleStatementDetailDTO> findSaleStatementOrderList(ErpSaleStatementSelectRQ saleStatementSelectRQ, Pagination pagination);

    List<ErpSaleStmtDetail> listSaleStmtDetailByOrder(Integer id);

    List<ErpSaleStatementDetailDTO> getSaleStatementInfo(Integer id);

    /**
     * 根据销售结算单id查询结算单明细信息
     * @param statementId
     * @returns
     */
    List<ErpSaleStatementDetailDTO> getSaleStatementsByStatementId(Integer statementId);

}

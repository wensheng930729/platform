package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.dto.ErpPurchaseStatementDetailDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseStmtDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseStmtDetail;

import java.util.List;

/**
 * <p>
 * 采购结算单结算详情 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseStmtDetailMapper extends BaseMapper<ErpPurchaseStmtDetail> {

    /**
     *
     * @param list
     * @return
     */
    int batchDelete(List<Integer> list);

    /**
     *
     * @param id
     * @return
     */
    List<ErpPurchaseStmtDetailDTO> listPurchaseStmtDetailByOrder(String id);

    /**
     * 根据采购结算id查询采购明细
     * @param id
     * @return
     */
    List<ErpPurchaseStatementDetailDTO> getPurchaseStatementsByStatementId(Integer statementId);
}

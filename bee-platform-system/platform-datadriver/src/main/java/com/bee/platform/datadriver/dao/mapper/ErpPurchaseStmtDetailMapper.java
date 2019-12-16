package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.entity.ErpPurchaseStmtDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.entity.ErpRepoReceiptDetail;

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
    List<ErpPurchaseStmtDetail> listPurchaseStmtDetailByOrder(String id);
}

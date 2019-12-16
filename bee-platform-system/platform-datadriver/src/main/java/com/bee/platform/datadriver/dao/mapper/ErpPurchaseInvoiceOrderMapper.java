package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpPurchaseInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseInvoiceOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.InvoiceOrderQueryRQ;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 采购发票 Mapper 接口
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
public interface ErpPurchaseInvoiceOrderMapper extends BaseMapper<ErpPurchaseInvoiceOrder> {

    /**
     *
     * @param rq
     * @param pagination
     * @return
     */
    List<ErpPurchaseInvoiceOrderInfoDTO> selectInvoiceOrderByCondition(InvoiceOrderQueryRQ rq, Pagination pagination);
}

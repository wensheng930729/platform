package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpSaleInvoiceOrderInfoDTO;
import com.bee.platform.datadriver.entity.ErpSaleInvoiceOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.InvoiceOrderQueryRQ;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 销售发票 Mapper 接口
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
public interface ErpSaleInvoiceOrderMapper extends BaseMapper<ErpSaleInvoiceOrder> {

    /**
     *
     * @param map
     * @param pagination
     * @return
     */
    List<ErpSaleInvoiceOrderInfoDTO> selectInvoiceOrderByCondition(InvoiceOrderQueryRQ rq, Pagination pagination);
}

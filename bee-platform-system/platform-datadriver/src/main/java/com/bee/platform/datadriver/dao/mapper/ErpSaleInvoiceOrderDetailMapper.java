package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.dto.ErpInvoiceOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpSaleInvoiceOrderDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 销售发票明细 Mapper 接口
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-01
 */
public interface ErpSaleInvoiceOrderDetailMapper extends BaseMapper<ErpSaleInvoiceOrderDetail> {

    /**
     *
     * @param id
     * @return
     */
    List<ErpInvoiceOrderDetailDTO> listInvoiceOrderDetail(String id);
}

package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.SaleInvoiceDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleInvoice;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.rq.SaleInvoiceListRQ;

import java.util.List;

/**
 * <p>
 * 销售发票 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSaleInvoiceMapper extends BaseMapper<DinasSaleInvoice> {

    SaleInvoiceDTO findInfo(Integer id);

    List<SaleInvoiceDTO> findList(SaleInvoiceListRQ rq, Pagination pagination);

}

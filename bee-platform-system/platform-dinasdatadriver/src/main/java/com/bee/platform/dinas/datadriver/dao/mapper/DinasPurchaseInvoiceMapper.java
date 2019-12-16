package com.bee.platform.dinas.datadriver.dao.mapper;

import com.bee.platform.dinas.datadriver.dto.DinasPurchaseInvoiceDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseInvoice;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseInvoiceQueryRQ;

import java.util.List;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;

/**
 * <p>
 * 采购发票 Mapper 接口
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
public interface DinasPurchaseInvoiceMapper extends BaseMapper<DinasPurchaseInvoice> {

	List<DinasPurchaseInvoiceDTO> queryList(DinasPurchaseInvoiceQueryRQ rq, Pagination pagination);

}

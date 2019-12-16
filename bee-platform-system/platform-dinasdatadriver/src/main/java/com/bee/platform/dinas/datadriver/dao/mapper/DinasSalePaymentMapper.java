package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.DinasSalePaymentDTO;
import com.bee.platform.dinas.datadriver.dto.SalePaymentListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSalePayment;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.rq.SalePaymentListRQ;

import java.util.List;

/**
 * <p>
 * 销售回款 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSalePaymentMapper extends BaseMapper<DinasSalePayment> {

    DinasSalePaymentDTO findInfo(Integer id);

    List<SalePaymentListDTO> findList(SalePaymentListRQ rq, Pagination pagination);
}

package com.bee.platform.dinas.datadriver.dao.mapper;

import com.bee.platform.dinas.datadriver.dto.DinasPurchasePayDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchasePay;
import com.bee.platform.dinas.datadriver.rq.DinasPurchasePayQueryRQ;

import java.util.List;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;

/**
 * <p>
 * 采购付款单 Mapper 接口
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-08-13
 */
public interface DinasPurchasePayMapper extends BaseMapper<DinasPurchasePay> {

	List<DinasPurchasePayDTO> queryList(DinasPurchasePayQueryRQ rq, Pagination pagination);


}

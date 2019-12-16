package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.DinasInspectionGoodsSearchDTO;
import com.bee.platform.dinas.datadriver.dto.DinasProductListDTO;
import com.bee.platform.dinas.datadriver.dto.DinasProductSpecListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasInspectionGoods;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.rq.DinasGetProductListRQ;
import com.bee.platform.dinas.datadriver.rq.DinasGetProductSpecListRQ;
import com.bee.platform.dinas.datadriver.rq.DinasInspectionGoodsSearchRQ;

import java.util.List;

/**
 * <p>
 * 验货磅单表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasInspectionGoodsMapper extends BaseMapper<DinasInspectionGoods> {

    List<DinasInspectionGoodsSearchDTO> searchInspectionGoodsByCondition(DinasInspectionGoodsSearchRQ rq, Pagination pagination);

    List<DinasProductListDTO> getProductList(DinasGetProductListRQ rq);

    List<DinasProductSpecListDTO> getProductSpecList(DinasGetProductSpecListRQ rq);
}

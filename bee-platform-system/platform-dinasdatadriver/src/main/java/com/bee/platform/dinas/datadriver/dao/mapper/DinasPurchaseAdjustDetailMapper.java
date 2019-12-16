package com.bee.platform.dinas.datadriver.dao.mapper;

import com.bee.platform.dinas.datadriver.dto.DinasPurchaseAdjustDetailDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseAdjustDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 采购调价明细表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasPurchaseAdjustDetailMapper extends BaseMapper<DinasPurchaseAdjustDetail> {

    /**
     * 查询条件明细
     * @param id
     * @return
     */
    List<DinasPurchaseAdjustDetailDTO> listPurchaseAdjustDetail(Integer id);
}

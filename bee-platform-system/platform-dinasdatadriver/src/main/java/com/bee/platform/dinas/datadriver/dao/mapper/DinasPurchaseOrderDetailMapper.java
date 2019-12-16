package com.bee.platform.dinas.datadriver.dao.mapper;

import com.bee.platform.dinas.datadriver.dto.DinasPurchaseOrderDetailDTO;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseOrderDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 采购合同明细 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasPurchaseOrderDetailMapper extends BaseMapper<DinasPurchaseOrderDetail> {

    /**
     * 采购合同明细列表
     * @param id
     * @return
     */
    List<DinasPurchaseOrderDetailDTO> listPurchaseOrderDetail(Integer id);
}

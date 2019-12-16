package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseOrderDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 采购单明细 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseOrderDetailMapper extends BaseMapper<ErpPurchaseOrderDetail> {

    /**
     *
     * @param id
     * @return
     */
    List<ErpPurchaseOrderDetailDTO> selectPurchaseOrderDetail(Pagination pagination, String id);

    List<ErpPurchaseOrderDetailDTO> selectPurchaseDetailForNum(Integer id);
}

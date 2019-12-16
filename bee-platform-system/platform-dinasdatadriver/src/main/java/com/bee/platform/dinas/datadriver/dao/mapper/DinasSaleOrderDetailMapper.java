package com.bee.platform.dinas.datadriver.dao.mapper;

import com.bee.platform.dinas.datadriver.dto.SaleOrderDetailDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleOrderDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;

/**
 * <p>
 * 销售合同明细 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSaleOrderDetailMapper extends BaseMapper<DinasSaleOrderDetail> {

    List<SaleOrderDetailDTO> findInfoByOrderId(Integer orderId);
}

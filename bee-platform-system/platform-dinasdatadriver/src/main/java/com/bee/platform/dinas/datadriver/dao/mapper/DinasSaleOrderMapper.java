package com.bee.platform.dinas.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.dinas.datadriver.dto.SaleOrderDTO;
import com.bee.platform.dinas.datadriver.dto.SaleOrderDetailDTO;
import com.bee.platform.dinas.datadriver.dto.SaleOrderListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.dinas.datadriver.rq.SaleOrderListRQ;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 销售合同 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSaleOrderMapper extends BaseMapper<DinasSaleOrder> {

    SaleOrderDTO findInfo(Integer id);

    List<SaleOrderListDTO> findSaleOrders (SaleOrderListRQ rq, Pagination pagination);

    List<SaleOrderDTO> findOrdersByCompanyId(Integer companyId);

    List<SaleOrderDetailDTO> findProductsById(Integer id);

    List<SaleOrderDetailDTO> findSpecsByProductId(@Param("orderId") Integer orderId,
                                                  @Param("productId") Integer productId);

}

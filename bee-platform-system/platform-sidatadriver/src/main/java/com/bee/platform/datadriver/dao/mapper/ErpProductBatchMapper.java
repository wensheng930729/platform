package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.dto.ErpProductBatchListDTO;
import com.bee.platform.datadriver.entity.ErpProductBatch;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 产品批次分类项 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-07-12
 */
public interface ErpProductBatchMapper extends BaseMapper<ErpProductBatch> {

    /**
     * 根据采购订单id查询批次
     * @param orderId
     * @return
     */
    List<ErpProductBatchListDTO> getPurchaseBatchsByOrderId(@Param("orderId") Integer orderId);

    /**
     * 根据销售订单id查询批次
     * @param orderId
     * @return
     */
    List<ErpProductBatchListDTO> getSaleBatchByOrderId(@Param("orderId")Integer orderId);
}

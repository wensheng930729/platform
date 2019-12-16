package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderInfoDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseOrderInfoDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseOrder;
import com.bee.platform.datadriver.entity.ErpRepoReceiptDetail;
import com.bee.platform.datadriver.rq.OrderQueryRQ;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 采购订单 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseOrderMapper extends BaseMapper<ErpPurchaseOrder> {

    /**
     *
     * @param rq
     * @param pagination
     * @return
     */
    List<ErpPurchaseOrderInfoDTO> selectPurchaseOrderByCondition(OrderQueryRQ rq, Pagination pagination);

    /**
     *
     * @param id
     * @return
     */
    // List<ErpPurchaseOrderInfoDetailDTO> selectByOrderId(Integer id);

    /**
     *
     * @param id
     * @return
     */
    ErpPurchaseOrderDTO getOrderById(Integer id);

    /**
     * @Description 根据采购订单id获取采购收货详情
     * @Param null
     * @Date 2019/6/3 10:05
     * @Author xin.huang
     * @Return
     */
    List<ErpRepoReceiptDetail> getPurchaseGoodsList(Integer orderId);

    /***
     * @descriptin 根据采购订单id获取订单及产品批次信息
     * @author xin.huang
     * @param id
     * @date 2019/7/25
     * @return
     */
    List<ErpPurchaseOrderInfoDetailDTO> findByOrderId(Integer id);
}

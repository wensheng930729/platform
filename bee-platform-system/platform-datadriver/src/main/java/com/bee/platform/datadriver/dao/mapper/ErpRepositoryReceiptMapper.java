package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpInventoryFlowDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseGoodsDetailDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryReceiptProductOutSearchDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryReceiptRawInSearchDTO;
import com.bee.platform.datadriver.entity.ErpRepositoryReceipt;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.*;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpRepositoryReceiptMapper extends BaseMapper<ErpRepositoryReceipt> {

    /**
     * @Description 条件查询采购收货单列表
     * @Param purchaseGoodsOrderSelectRQ
     * @Param pagination
     * @Date 2019/5/30 14:45
     * @Author xin.huang
     * @Return
     */
    List<ErpPurchaseGoodsDetailDTO> findPurchaseGoodsOrderList(ErpPurchaseGoodsOrderSelectRQ purchaseGoodsOrderSelectRQ, Pagination pagination);


    List<ErpInventoryFlowDTO> searchInventoryFlowByCondition(Pagination pagination,ErpInventoryFlowSearchRQ rq);

    List<ErpPurchaseGoodsDetailDTO> findStatementDeliveryOrderList(ErpStatementDeliveryOrderSelectRQ statementDeliveryOrderSelectRQ, Pagination pagination);

    List<ErpRepositoryReceiptRawInSearchDTO> searchRepositoryReceiptRawInByCondition(ErpRepositoryReceiptRawInSearchRQ rq, Pagination pagination);

    List<ErpRepositoryReceiptProductOutSearchDTO> searchRepositoryReceiptProductOutByCondition(ErpRepositoryReceiptProductOutSearchRQ rq, Pagination pagination);
}

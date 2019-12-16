package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDTO;
import com.bee.platform.datadriver.dto.ErpReceiptOrderSearchDTO;
import com.bee.platform.datadriver.entity.ErpReceiptOrder;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.datadriver.rq.ErpReceiptSearchRQ;

import java.util.List;

/**
 * <p>
 * 销售收款单主表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpReceiptOrderMapper extends BaseMapper<ErpReceiptOrder> {
    // 弃用

    List<ErpReceiptOrderDTO> searchReceiptByCondition(ErpReceiptSearchRQ rq, Pagination pagination);

    /**
     * 条件查询销售收款列表
     * @param rq
     * @param pagination
     * @return
     */
    List<ErpReceiptOrderSearchDTO> searchReceiptOrderByCondition(ErpReceiptSearchRQ rq,Pagination pagination);



}

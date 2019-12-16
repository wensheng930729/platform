package com.bee.platform.datadriver.service;

import com.bee.platform.datadriver.dto.ErpPurchaseStmtDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseStmtDetail;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * <p>
 * 采购结算单结算详情 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseStmtDetailService extends IService<ErpPurchaseStmtDetail> {

    /**
     *
     * @param id
     * @return
     */
    List<ErpPurchaseStmtDetailDTO> listPurchaseStmtDetailByOrder(String id);
}

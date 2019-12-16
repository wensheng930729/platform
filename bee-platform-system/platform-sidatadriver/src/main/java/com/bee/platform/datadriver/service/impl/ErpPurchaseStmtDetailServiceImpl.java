package com.bee.platform.datadriver.service.impl;

import com.bee.platform.datadriver.dto.ErpPurchaseStmtDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseStmtDetail;
import com.bee.platform.datadriver.dao.mapper.ErpPurchaseStmtDetailMapper;
import com.bee.platform.datadriver.service.ErpPurchaseStmtDetailService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 采购结算单结算详情 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Service
public class ErpPurchaseStmtDetailServiceImpl extends ServiceImpl<ErpPurchaseStmtDetailMapper, ErpPurchaseStmtDetail> implements ErpPurchaseStmtDetailService {

    @Autowired
    private ErpPurchaseStmtDetailMapper purchaseStmtDetailMapper;

    /**
     *
     * @param id
     * @return
     */
    @Override
    public List<ErpPurchaseStmtDetailDTO> listPurchaseStmtDetailByOrder(String id) {
        return purchaseStmtDetailMapper.listPurchaseStmtDetailByOrder(id);
    }
}

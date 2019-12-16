package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDTO;
import com.bee.platform.datadriver.dto.ErpReceiptOrderSearchDTO;
import com.bee.platform.datadriver.entity.ErpReceiptOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpReceiptOrderRQ;
import com.bee.platform.datadriver.rq.ErpReceiptOrderUpdateRQ;
import com.bee.platform.datadriver.rq.ErpReceiptSearchRQ;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

/**
 * <p>
 * 销售收款单主表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpReceiptOrderService extends IService<ErpReceiptOrder> {

    /**
     * 保存销售收款单
     * @param rq 请求参数
     */
    Integer saveReceiptOrder(AuthPlatformUserInfo userInfo , ErpReceiptOrderRQ rq);

    /**
     * 查询销售收款单信息
     * @param id 收款单id
     * @return
     */
    ErpReceiptOrderDTO getReceiptOrderById(Integer id);

    /**
     *
     * @param userInfo
     * @param id
     * @param state
     */
    void updateReceiptOrderState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    ResponseResult<List<ErpReceiptOrderSearchDTO>> searchReceiptByCondition(ErpReceiptSearchRQ rq, Page page, Integer companyId);


    void deleteReceiptOrderById(AuthPlatformUserInfo userInfo, Integer id);
}

package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpReceiptOrderDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpReceiptOrderDetailRQ;

import java.util.List;

/**
 * <p>
 * 销售收款单收款详情表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpReceiptOrderDetailService extends IService<ErpReceiptOrderDetail> {

    void deleteDetail(AuthPlatformUserInfo userInfo,Integer id);

    Integer saveReceiptOrderDetail(AuthPlatformUserInfo userInfo, ErpReceiptOrderDetailRQ rq);

    ErpReceiptOrderDetailDTO getReceiptOrderDetailById(Integer id);

    List<ErpReceiptOrderDetailDTO> getReceiptOrderDetailByOrderId(Integer id);
}

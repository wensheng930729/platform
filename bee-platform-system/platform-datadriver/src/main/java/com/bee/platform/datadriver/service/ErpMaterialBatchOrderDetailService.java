package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpMaterialBatchDetailsDTO;
import com.bee.platform.datadriver.dto.ErpMaterialBatchOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrderDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpMaterialBatchOrderDetailRQ;

import java.util.List;

/**
 * <p>
 * 料批明细表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpMaterialBatchOrderDetailService extends IService<ErpMaterialBatchOrderDetail> {

    Integer saveMaterialBatchOrderDetail(AuthPlatformUserInfo userInfo, ErpMaterialBatchOrderDetailRQ rq);

    void deleteMaterialBatchOrderDetailById(AuthPlatformUserInfo userInfo, Integer id);

    List<ErpMaterialBatchDetailsDTO> getMaterialBatchDetails(Integer id);

    ErpMaterialBatchOrderDetailDTO getMaterialBatchDetailById(Integer id);
}

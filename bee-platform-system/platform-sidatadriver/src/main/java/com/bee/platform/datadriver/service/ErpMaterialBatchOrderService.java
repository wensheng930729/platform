package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpMaterialBatchOrderRQ;
import com.bee.platform.datadriver.rq.ErpMaterialBatchSearchRQ;

import java.util.List;

/**
 * <p>
 * 料批主表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpMaterialBatchOrderService extends IService<ErpMaterialBatchOrder> {

    ResponseResult<List<ErpMaterialBatchSearchListDTO>> searchMaterialBatchByCondition( ErpMaterialBatchSearchRQ rq, Page page, Integer companyId);

    void updateMaterialBatchState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    Integer saveMaterialBatchOrder(AuthPlatformUserInfo userInfo, ErpMaterialBatchOrderRQ rq);

    void deleteMaterialBatchById(AuthPlatformUserInfo userInfo, Integer id);

    ErpMaterialBatchOrderDTO getMaterialBatchById(Integer id);

	ResponseResult<List<ErpMaterialBatchOrder>> getMaterialBatch(Integer productId, Integer companyId);

    List<ErpMaterialBatchListDTO> getMaterialBatchList(AuthPlatformUserInfo userInfo,String sysToken);

    List<ErpMaterialBatchListDTO> getMaterialBatchListByCompanyId(Integer companyId);

    List<ErpProductAndBatchListDTO> getMaterialBatchDetailProductList(Integer id);
}

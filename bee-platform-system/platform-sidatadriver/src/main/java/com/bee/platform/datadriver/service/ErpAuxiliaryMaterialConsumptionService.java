package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpAuxiliaryMaterialConsumptionDTO;
import com.bee.platform.datadriver.entity.ErpAuxiliaryMaterialConsumption;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpAuxiliaryMaterialConsumptionRQ;
import com.bee.platform.datadriver.rq.ErpAuxiliaryMaterialConsumptionSearchRQ;

import java.util.List;

/**
 * <p>
 * 辅材消耗表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpAuxiliaryMaterialConsumptionService extends IService<ErpAuxiliaryMaterialConsumption> {

    ResponseResult<List<ErpAuxiliaryMaterialConsumptionDTO>> searchAuxiliaryMaterialConsumptionByCondition( ErpAuxiliaryMaterialConsumptionSearchRQ rq, Page page, Integer companyId);

    Integer saveAuxiliaryMaterialConsumption(AuthPlatformUserInfo userInfo, ErpAuxiliaryMaterialConsumptionRQ rq);

    void deleteAuxiliaryMaterialConsumptionById(AuthPlatformUserInfo userInfo, Integer id);

    ErpAuxiliaryMaterialConsumptionDTO getAuxiliaryMaterialConsumptionById(Integer id);
}

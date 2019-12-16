package com.bee.platform.dinas.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.entity.DinasPurchaseAdjust;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.dinas.datadriver.rq.DinasPurchaseAdjustRQ;

/**
 * <p>
 * 采购调价主表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasPurchaseAdjustService extends IService<DinasPurchaseAdjust> {

    /**
     * 添加采购调价
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> addPurchaseAdjust(AuthPlatformUserInfo userInfo, DinasPurchaseAdjustRQ rq);
}

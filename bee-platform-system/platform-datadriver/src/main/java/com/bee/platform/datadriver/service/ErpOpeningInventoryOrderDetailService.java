package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderDetailDTO;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrderDetail;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpOpeningInventoryOrderDetailRQ;

import java.util.List;

/**
 * <p>
 * 期初库存明细表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpOpeningInventoryOrderDetailService extends IService<ErpOpeningInventoryOrderDetail> {

    /**
     * 查询期初库存明细列表
     * @param id
     * @return
     */
    List<ErpOpeningInventoryOrderDetail> listOpeningInventoryOrderDetail(String id);

    /**
     * 保存期初库存明细
     * @param userInfo
     * @param rq
     * @return
     */
    Integer saveOpeningInventoryOrderDetail(AuthPlatformUserInfo userInfo, ErpOpeningInventoryOrderDetailRQ rq);

    void deleteOpeningInventoryOrderDetailById(AuthPlatformUserInfo userInfo, Integer id);

    ErpOpeningInventoryOrderDetailDTO getOpeningInventoryOrderDetailById(Integer id);
}

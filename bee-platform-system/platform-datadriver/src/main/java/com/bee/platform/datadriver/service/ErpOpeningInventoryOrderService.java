package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderDTO;
import com.bee.platform.datadriver.dto.ErpOpeningInventoryOrderSearchListDTO;
import com.bee.platform.datadriver.dto.ErpOpeningInventorySearchDTO;
import com.bee.platform.datadriver.entity.ErpOpeningInventoryOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.ErpOpeningInventoryOrderSaveRQ;
import com.bee.platform.datadriver.rq.ErpOpeningInventorySearchRQ;
import com.bee.platform.datadriver.rq.OpeningInventoryOrderQueryRQ;

import java.util.List;

/**
 * <p>
 * 期初库存主表 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
public interface ErpOpeningInventoryOrderService extends IService<ErpOpeningInventoryOrder> {

    /**
     * 条件查询期初库存列表
     * @param
     * @param rq
     * @return
     */
    ResponseResult<List<ErpOpeningInventoryOrderSearchListDTO>> searchOpeningInventoryOrderByCondition(AuthPlatformUserInfo userInfo, OpeningInventoryOrderQueryRQ rq, Page page,String sysToken);

    /**
     * 获取期初库存信息
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<ErpOpeningInventoryOrder> getOpeningInventoryOrder(AuthPlatformUserInfo userInfo, String id);

    /**
     * 删除期初库存信息
     * @param userInfo
     * @param id
     * @return
     */
    void deleteOpeningInventoryOrderById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 确认期初库存信息
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<Integer> confirmOpeningInventoryOrder(AuthPlatformUserInfo userInfo, String id);

    /**
     * 保存期初库存
     * @param userInfo
     * @param rq
     * @return
     */
    Integer saveOpeningInventoryOrder(AuthPlatformUserInfo userInfo, ErpOpeningInventoryOrderSaveRQ rq);

    /**
     * 修改期初库存状态
     * @param userInfo
     * @param id
     * @param state
     */
    void updateOpeningInventoryState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    /**
     * 根据id查看料批详情
     * @param id
     * @return
     */
    ErpOpeningInventoryOrderDTO getOpeningInventoryById(Integer id);

    ResponseResult<List<ErpOpeningInventorySearchDTO>> searchOpeningInventoryByCondition(ErpOpeningInventorySearchRQ rq, Page page, Integer companyId);
}

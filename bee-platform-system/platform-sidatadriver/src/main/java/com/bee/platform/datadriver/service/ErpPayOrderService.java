package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.entity.ErpPayOrder;
import com.bee.platform.datadriver.rq.ErpPayOrderSaveRQ;
import com.bee.platform.datadriver.rq.PayOrderRQ;

import java.util.List;

/**
 * <p>
 * 付款单 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPayOrderService extends IService<ErpPayOrder> {

    /**
     * 分页查询付款单列表
     * @param pagination
     * @param rq
     * @return
     */
    List<ErpPayOrder> listErpPayOrder(Pagination pagination, PayOrderRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 删除付款单
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<Integer> deleteErpPayOrder(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 确认付款单
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> confirmErpPayOrder(AuthPlatformUserInfo userInfo, String id);

    /**
     * 保存付款单
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> saveErpPayOrder(AuthPlatformUserInfo userInfo , ErpPayOrderSaveRQ rq);

    /**
     * 根据id查询付款单
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<ErpPayOrder> getErpPayOrderById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据采购单id查询付款单
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<List<ErpPayOrder>> getErpPayOrderByPurchase(AuthPlatformUserInfo userInfo, Integer id);
}

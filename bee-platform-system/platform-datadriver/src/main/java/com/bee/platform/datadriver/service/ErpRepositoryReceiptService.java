package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpRepositoryReceipt;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.*;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpRepositoryReceiptService extends IService<ErpRepositoryReceipt> {

    /**
     * @Description 新增采购收货单
     * @Param repositoryReceiptRQ
     * @Date 2019/5/28 17:17
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> addPurchaseGoodsOrder(AuthPlatformUserInfo simpleUserInfo, RepositoryReceiptRQ repositoryReceiptRQ);

    /**
     * @Description 更新采购收货单
     * @Param repositoryReceiptRQ
     * @Date 2019/5/29 11:39
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> updatePurchaseGoodsOrder(AuthPlatformUserInfo simpleUserInfo, RepositoryReceiptRQ repositoryReceiptRQ);

     /**
      * @Description 批量删除采购单
      * @Param ids
      * @Date 2019/5/29 15:42
      * @Author xin.huang
      * @Return
      */
    ResponseResult<ResCodeEnum> batchDeletePurchaseGoodsOrder(String ids);

    /**
     * @Description 查看采购收货单
     * @Param id
     * @Date 2019/5/29 16:35
     * @Author xin.huang
     * @Return
     */
    ErpPurchaseGoodsOrderDTO findPurchaseGoodsOrder(Integer id);

    /**
     * @Description 条件查询采购收货单列表
     * @Param purchaseGoodsOrderSelectRQ
     * @Param pagination
     * @Date 2019/5/30 14:45
     * @Author xin.huang
     * @Return
     */
    List<ErpPurchaseGoodsDetailDTO> findPurchaseGoodsOrderList(AuthPlatformUserInfo simpleUserInfo, ErpPurchaseGoodsOrderSelectRQ purchaseGoodsOrderSelectRQ, Pagination pagination);

    /**
     * @Description 更新采购收货单状态
     * @Param id
     * @Param state
     * @Date 2019/5/30 15:23
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> updateState(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state);

    /**
     * @Description 新增销售发货单
     * @Param repositoryReceiptRQ
     * @Date 2019/5/31 10:01
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> addStatementDeliveryOrder(AuthPlatformUserInfo simpleUserInfo, RepositoryReceiptRQ repositoryReceiptRQ);

    /**
     * @Description 更新销售发货单
     * @Param id
     * @Param repositoryReceiptRQ
     * @Date 2019/5/31 10:18
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> updateStatementDeliveryOrder(AuthPlatformUserInfo simpleUserInfo,  RepositoryReceiptRQ repositoryReceiptRQ);

    /**
     * @Description 批量删除销售发货单
     * @Param ids
     * @Date 2019/5/31 10:23
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> batchStatementDeliveryOrder(String ids);

    /**
     * @Description 查看销售发货单
     * @Param id
     * @Date 2019/5/29 16:35
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ErpPurchaseGoodsOrderDTO> findStatementDeliveryOrder(Integer id);

    /**
     * @Description 条件查询销售发货单列表
     * @Param null
     * @Date 2019/5/31 11:26
     * @Author xin.huang
     * @Return 
     */
    List<ErpPurchaseGoodsDetailDTO> findStatementDeliveryOrderList(ErpStatementDeliveryOrderSelectRQ statementDeliveryOrderSelectRQ,
                                                                   Pagination pagination, String sysToken);

    ResponseResult<ErpInventoryFlowSearchDTO> searchInventoryFlowByCondition(ErpInventoryFlowSearchRQ rq, Page page, Integer companyId);

    /**
     * @Description 更新销售发货单状态
     * @Param id
     * @Param state
     * @Date 2019/5/30 15:23
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> updateStateStatementDelivery(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state);

    Integer saveRepositoryReceiptRawIn(AuthPlatformUserInfo userInfo, RepositoryReceiptRawInRQ rq);

    void updateRepositoryReceiptRawInState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    void deleteRepositoryReceiptRawInById(AuthPlatformUserInfo userInfo, Integer id);

    ErpRepositoryReceiptRawInDTO getRepositoryReceiptRawInById(Integer id);

    Integer saveRepositoryReceiptProductOut(AuthPlatformUserInfo userInfo, ErpRepositoryReceiptProductOutRQ rq);

    void updateRepositoryReceiptProductOutState(AuthPlatformUserInfo userInfo, Integer id, Integer state);

    void deleteRepositoryReceiptProductOutById(AuthPlatformUserInfo userInfo, Integer id);

    ErpRepositoryReceiptProductOutDTO getRepositoryReceiptProductOutById(Integer id);

    ResponseResult<List<ErpRepositoryReceiptRawInSearchDTO>> searchRepositoryReceiptRawInByCondition(ErpRepositoryReceiptRawInSearchRQ rq, Page page, Integer companyId);

    ResponseResult<List<ErpRepositoryReceiptProductOutSearchDTO>> searchRepositoryReceiptProductOutByCondition(ErpRepositoryReceiptProductOutSearchRQ rq, Page page, Integer companyId);

    ResponseResult<List<RepoReceiptDetailDTO>> getSaleDeliveryInfo(Integer id);
}

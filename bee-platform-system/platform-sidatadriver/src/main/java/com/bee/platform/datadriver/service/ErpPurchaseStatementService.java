package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpPurchaseStatementDTO;
import com.bee.platform.datadriver.dto.ErpPurchaseStatementDetailDTO;
import com.bee.platform.datadriver.entity.ErpPurchaseStatement;
import com.bee.platform.datadriver.rq.ErpPurchaseStatementRQ;
import com.bee.platform.datadriver.rq.PurchaseStatementRQ;

import java.util.List;

/**
 * <p>
 * 采购结算单 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpPurchaseStatementService extends IService<ErpPurchaseStatement> {

    /**
     * @Description 新增采购结算
     * @Param purchaseStatementRQ
     * @Date 2019/5/30 16:27
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> add(AuthPlatformUserInfo simpleUserInfo, PurchaseStatementRQ purchaseStatementRQ);

    /**
     * @Description 新增采购结算
     * @Param id
     * @Param purchaseStatementRQ
     * @Date 2019/5/30 19:09
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> update(AuthPlatformUserInfo simpleUserInfo, PurchaseStatementRQ purchaseStatementRQ);

    /**
     * @Description 批量删除采购结算单
     * @Param ids
     * @Date 2019/5/30 19:30
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ResCodeEnum> batchDelete(String ids);

    /**
     * @Description 查询采购结算单详情
     * @Param id
     * @Date 2019/5/30 19:54
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ErpPurchaseStatementDTO> findStatementInfo(Integer id);

    /**
     * @Description 更新采购结算单状态
     * @Param id
     * @Param state
     * @Date 2019/5/30 20:31
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> updateState(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state);

    /**
     * @Description 条件查询采购结算单
     * @Param erpPurchaseStatementRQ
     * @Param pagination
     * @Date 2019/5/30 20:50
     * @Author xin.huang
     * @Return
     */
    List<ErpPurchaseStatementDetailDTO> findStatementList(Integer companyId, ErpPurchaseStatementRQ erpPurchaseStatementRQ, Pagination pagination);

    /**
     * @Description 根据采购合同id查询验收情况
     * @Param orderId
     * @Param productBatchId
     * @Date 2019/6/6 9:57
     * @Author xin.huang
     * @Return
     */
    ResponseResult findTestReportInfo(Integer orderId, Integer productBatchId);

}

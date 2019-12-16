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



    ResponseResult<List<ErpInventoryFlowDTO>> searchInventoryFlowByCondition(ErpInventoryFlowSearchRQ rq, Page page, Integer companyId);


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

package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailProductOutDTO;
import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailRawInDTO;
import com.bee.platform.datadriver.dto.RepoReceiptDetailDTO;
import com.bee.platform.datadriver.entity.ErpRepoReceiptDetail;
import com.bee.platform.datadriver.rq.ReceiptDetailRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailProductOutRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailRQ;
import com.bee.platform.datadriver.rq.RepoReceiptDetailRawInRQ;

import java.util.List;

/**
 * <p>
 * 仓库单明细 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpRepoReceiptDetailService extends IService<ErpRepoReceiptDetail> {

    /**
     *
     * @param rq
     * @return
     */
    List<RepoReceiptDetailDTO> listRepoReceiptDetail(ReceiptDetailRQ rq);


    Integer saveRepoReceiptDetailRawIn(AuthPlatformUserInfo userInfo, RepoReceiptDetailRawInRQ rq);

    void deleteRepoReceiptDetailRawIn(AuthPlatformUserInfo userInfo, Integer id);


    List<ErpRepoReceiptDetailRawInDTO> getRepoReceiptDetailRawInList(Integer receiptId);

    Integer saveRepoReceiptDetailProductOut(AuthPlatformUserInfo userInfo, RepoReceiptDetailProductOutRQ rq);

    void deleteRepoReceiptDetailProductOutById(AuthPlatformUserInfo userInfo, Integer id);

    List<ErpRepoReceiptDetailProductOutDTO> getRepoReceiptDetailProductOutList(Integer receiptId);

    ErpRepoReceiptDetailRawInDTO getRepoReceiptDetailRawInById(Integer id);

    ErpRepoReceiptDetailProductOutDTO getRepoReceiptDetailProductOutById(Integer id);
}

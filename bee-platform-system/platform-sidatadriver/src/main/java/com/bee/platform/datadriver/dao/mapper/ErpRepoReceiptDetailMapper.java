package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailProductOutDTO;
import com.bee.platform.datadriver.dto.ErpRepoReceiptDetailRawInDTO;
import com.bee.platform.datadriver.dto.ErpSaleStatementOrderDetailDTO;
import com.bee.platform.datadriver.dto.RepoReceiptDetailDTO;
import com.bee.platform.datadriver.entity.ErpRepoReceiptDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import io.swagger.models.auth.In;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 仓库单明细 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpRepoReceiptDetailMapper extends BaseMapper<ErpRepoReceiptDetail> {

    int batchDeleteReceiptDetailByReceiptIds(List<Integer> ids);

    List<RepoReceiptDetailDTO> findReoReceiptDetailInfo(Integer receiptId);

    List<RepoReceiptDetailDTO> findStatementDeliveryOrderInfo(Integer receiptId);

    /**
     *
     * @param orderId
     * @param businessType
     * @return
     */
    List<RepoReceiptDetailDTO> selectByOrderId(@Param("orderId") String orderId, @Param("businessType") String businessType);

    List<ErpSaleStatementOrderDetailDTO> findSaleStatementOrderInfo(Integer id);

    List<ErpRepoReceiptDetailRawInDTO> getRepoReceiptDetailRawInList(Integer receiptId);

    List<ErpRepoReceiptDetailProductOutDTO> getRepoReceiptDetailProductOutList(Integer receiptId);

    /**
     *
     * @param id
     * @return
     */
    List<ErpRepoReceiptDetail> listByOrderId(@Param("type") String type, @Param("id") Integer id);

    List<RepoReceiptDetailDTO> getSaleDeliveryInfo(Integer id);
}

package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.dto.ErpProductAndBatchListDTO;
import com.bee.platform.datadriver.dto.ErpProductBoxDTO;
import com.bee.platform.datadriver.dto.GetCompanyNameByIdsDTO;
import org.apache.ibatis.annotations.Param ;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName CommonMapper
 * @Description 功能描述
 * @Date 2019/6/7 11:24
 **/
public interface CommonMapper {


    String getCustomerNameById(Integer customerId);

    String getRepositoryNameById(Integer repositoryId);

    String getTestCodeById(Integer testId);

    String getFurnaceNameById(Integer furnaceId);

    String getMaterialBatchNameById(Integer mId);

    String getPurchaseOrderCodeById(Integer poId);

    String getTestResultById(Integer testId);

    String getSaleOrderCodeById(Integer soId);

    /**
     * 辅材消耗 从成品入库获取产量
     * @param companyId
     * @param furnaceId
     * @param time
     * @return
     */
    BigDecimal getOutPutFromWHO(@Param("companyId") Integer companyId,@Param("furnaceId") Integer furnaceId,@Param("time") Date time);

    List<ErpProductAndBatchListDTO> getProductListByPurchaseId(Integer purchaseId);

    List<ErpProductAndBatchListDTO> getProductListBySaleId(Integer saleId);

    String getProductNameById(Integer productId);



}

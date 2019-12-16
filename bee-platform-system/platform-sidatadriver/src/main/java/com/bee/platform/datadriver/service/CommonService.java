package com.bee.platform.datadriver.service;

import com.bee.platform.datadriver.dto.*;

import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName CommonService
 * @Description 功能描述
 * @Date 2019/6/2 10:29
 **/



public interface CommonService {


    String generateOrderId(Integer type);

    List<ErpRepositoryBoxDTO> getRepositoryListByCompanyId(Integer companyId);

    List<ErpProductBoxDTO> getProductListByCompanyId(Integer companyId);

    List<ErpTestReportSearchListDTO> searchTestReportList(String testCode);

    List<ErpTestReportSearchListDTO> searchTestReportListAddCompanyId(String testCode, Integer companyId);

    List<ErpFurnaceBoxDTO> getFurnaceListByCompanyId(Integer companyId);

    List<ErpPurchaseListDTO> getPurchaseListByUserInfo(String sysToken);

    List<ErpPurchaseListDTO> getPurchaseListByUserCompanyId(Integer orgId);

    List<ErpSaleListDTO> getSaleListByUserCompanyId(Integer orgId);

    List<ErpProductAndBatchListDTO> getProductListByPurchaseId(Integer purchaseId);

    List<ErpProductAndBatchListDTO> getProductListBySaleId(Integer saleId);

    List<ErpTestReportSearchByConditionDTO> searchTestReportListAddProductId(String testCode, Integer productId);

    List<ErpTestReportSearchByConditionDTO> searchTestReportListByProductIdAndCompanyId(Integer companyId, Integer productId);

    /**
     * 根据个数在前面用0补齐
     * @param code
     * @param length 总长度
     * @return
     */
    public String getCode(String code, Integer length);

    /**
     * 通过redisKey从redis获取value
     * 如果value为空则设置为1
     * @param redisKey
     * @return
     */
    public int getRedisCode(String redisKey);
}

package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.datadriver.dao.mapper.CommonMapper;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.service.*;
import com.bee.platform.user.authority.dto.AuthEnterpriseFlatDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.time.Instant;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName CommonServiceImpl
 * @Description 功能描述
 * @Date 2019/6/2 10:30
 **/

@Slf4j
@Service
public class CommonServiceImpl implements CommonService {

    @Autowired
    private ErpRepositoryService erpRepositoryService;
    @Autowired
    private ErpProductService erpProductService;
    @Autowired
    private ErpTestReportService erpTestReportService;
    @Autowired
    private ErpFurnaceService erpFurnaceService;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpPurchaseOrderService erpPurchaseOrderService;
    @Autowired
    private ErpSaleOrderService erpSaleOrderService;
    @Autowired
    private CommonMapper commonMapper;
    @Autowired
    private JedisService jedisService;

    /**
     * 根据业务类型生成单号
     *
     * @param type
     * @return
     */
    @Override
    public String generateOrderId(Integer type) {
        LocalDate now = LocalDate.now();
        String timeString = now.toString().replace("-", "");
        String mills = String.valueOf(Instant.now().toEpochMilli());
        String sequence = mills.substring(mills.length() - 6);

        //业务id
        StringBuffer orderId = new StringBuffer();
        switch (type) {
            case 0:
                // 采购订单
                orderId.append("PO");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 1:
                // 采购收货(原料入库)
                orderId.append("CGRK");
                orderId.append(timeString);
                orderId.append(sequence);
                break;

            case 2:
                // 采购结算
                orderId.append("CGJS");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 3:
                // 采购付款
                orderId.append("CGFK");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 4:
                // 销售订单
                orderId.append("SO");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 5:
                // 销售发货(成品出库)
                orderId.append("XSCK");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 6:
                // 销售结算
                orderId.append("XSJS");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 7:
                // 销售收款
                orderId.append("XSFK");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 8:
                // 期初单号
                orderId.append("QC");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 9:
                // 领料出库
                orderId.append("SCCK");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 10:
                // 成品入库
                orderId.append("SCRK");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 11:
                // 采购发票
                orderId.append("CGFP");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            case 12:
                // 销售发票
                orderId.append("XSFP");
                orderId.append(timeString);
                orderId.append(sequence);
                break;
            default:
                log.info("===输入的类型不匹配===" + type);
                throw new BusinessException(ResCodeEnum.FAILED, ExceptionMessageEnum.GENERATE_ORDER_ID_FAILED);

        }

        return orderId.toString();
    }

    /**
     * 根据企业id查询仓库列表
     *
     * @param companyId
     * @return
     */
    @Override
    public List<ErpRepositoryBoxDTO> getRepositoryListByCompanyId(Integer companyId) {

        List<ErpRepository> list = erpRepositoryService.selectList(new EntityWrapper<ErpRepository>().eq("status", 1)
                .eq("deleted", 0).eq("org_id", companyId));
        return BeanUtils.assemble(ErpRepositoryBoxDTO.class, list);
    }

    /**
     * 根据企业id查询企业下的产品列表
     *
     * @param companyId
     * @return
     */
    @Override
    public List<ErpProductBoxDTO> getProductListByCompanyId(Integer companyId) {

        List<ErpProduct> list = erpProductService.selectList(new EntityWrapper<ErpProduct>().eq("status", 1)
                .eq("deleted", 0).eq("enterprise_id", companyId));
        return BeanUtils.assemble(ErpProductBoxDTO.class, list);
    }


    /**
     * 模糊搜索化验单列表
     *
     * @param testCode
     * @return
     */
    @Override
    public List<ErpTestReportSearchListDTO> searchTestReportList(String testCode) {
        List<ErpTestReport> list = erpTestReportService.selectList(new EntityWrapper<ErpTestReport>()
                .eq("deleted", 0).like("code", testCode).orderBy("id", false));
        return BeanUtils.assemble(ErpTestReportSearchListDTO.class, list);
    }

    /**
     * 模糊搜索化验单列表加上公司id
     *
     * @param testCode
     * @param companyId
     * @return
     */
    @Override
    public List<ErpTestReportSearchListDTO> searchTestReportListAddCompanyId(String testCode, Integer companyId) {
        List<ErpTestReport> list = erpTestReportService.selectList(new EntityWrapper<ErpTestReport>()
                .eq("deleted", 0).eq("company", companyId).like("code", testCode).orderBy("id", false));
        return BeanUtils.assemble(ErpTestReportSearchListDTO.class, list);
    }


    /**
     * 根据产品id加模糊搜索化验单
     *
     * @param testCode
     * @param productId
     * @return
     */
    @Override
    public List<ErpTestReportSearchByConditionDTO> searchTestReportListAddProductId(String testCode, Integer productId) {

        List<ErpTestReport> list = erpTestReportService.selectList(new EntityWrapper<ErpTestReport>()
                .eq("deleted", 0).eq("product", productId).like("code", testCode).orderBy("id", false));

        List<ErpTestReportSearchByConditionDTO> dto = Lists.newArrayList();

        if (!CollectionUtils.isEmpty(list)) {
            for (ErpTestReport t : list) {
                ErpTestReportSearchByConditionDTO d = new ErpTestReportSearchByConditionDTO().setTestReportId(t.getId()).setCode(t.getCode()).setResult(t.getResult()).setProductId(productId);
                dto.add(d);
            }
        }

        return dto;
    }

    /**
     * 根据公司id加产品id查询化验单列表
     *
     * @param companyId
     * @param productId
     * @return
     */
    @Override
    public List<ErpTestReportSearchByConditionDTO> searchTestReportListByProductIdAndCompanyId(Integer companyId, Integer productId) {
        List<ErpTestReport> list = erpTestReportService.selectList(new EntityWrapper<ErpTestReport>()
                .eq("deleted", 0).eq("product", productId).eq("company", companyId).orderBy("id", false));


        List<ErpTestReportSearchByConditionDTO> dto = Lists.newArrayList();

        if (!CollectionUtils.isEmpty(list)) {
            for (ErpTestReport t : list) {
                ErpTestReportSearchByConditionDTO d = new ErpTestReportSearchByConditionDTO().setTestReportId(t.getId()).setCode(t.getCode()).setResult(t.getResult()).setProductId(productId);
                dto.add(d);
            }
        }

        return dto;
    }

    /**
     * 根据公司id查询炉号列表
     *
     * @param companyId
     * @return
     */
    @Override
    public List<ErpFurnaceBoxDTO> getFurnaceListByCompanyId(Integer companyId) {

        List<ErpFurnace> list = erpFurnaceService.selectList(new EntityWrapper<ErpFurnace>().eq("deleted", 0)
                .eq("status", 1).eq("org_id", companyId));
        return BeanUtils.assemble(ErpFurnaceBoxDTO.class, list);
    }


    /**
     * 根据登录人信息查询采购订单列表
     *
     * @param sysToken
     * @return
     */
    @Override
    public List<ErpPurchaseListDTO> getPurchaseListByUserInfo(String sysToken) {

        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByUser(sysToken).getObject();
        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
            log.info("未查询到当前用户企业信息！ 类：{} 方法：{}", "CommonServiceImpl", "getPurchaseListByUserInfo");
            return Lists.newArrayList();
        }
        List<Integer> ids = enterpriseFlatDTOS.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList());

        if (org.springframework.util.CollectionUtils.isEmpty(ids)) {
            return Lists.newArrayList();
        }

        List<ErpPurchaseOrder> list = erpPurchaseOrderService.selectList(new EntityWrapper<ErpPurchaseOrder>().eq("deleted", 0).in("company", ids));
        return BeanUtils.assemble(ErpPurchaseListDTO.class, list);
    }

    /**
     * 根据登录人公司id查询采购订单列表
     *
     * @param orgId
     * @return
     */
    @Override
    public List<ErpPurchaseListDTO> getPurchaseListByUserCompanyId(Integer orgId) {
        List<ErpPurchaseOrder> list = erpPurchaseOrderService.selectList(new EntityWrapper<ErpPurchaseOrder>().eq("deleted", 0).eq("company", orgId).orderBy("create_time", false));
        return BeanUtils.assemble(ErpPurchaseListDTO.class, list);
    }


    /**
     * 根据登录人公司id查询销售订单列表
     *
     * @param orgId
     * @return
     */
    @Override
    public List<ErpSaleListDTO> getSaleListByUserCompanyId(Integer orgId) {

        List<ErpSaleOrder> list = erpSaleOrderService.selectList(new EntityWrapper<ErpSaleOrder>().eq("deleted", 0).eq("company", orgId).orderBy("create_time", false));
        return BeanUtils.assemble(ErpSaleListDTO.class, list);
    }


    /**
     * 根据采购订单id查询明细里的产品列表
     *
     * @param purchaseId
     * @return
     */
    @Override
    public List<ErpProductAndBatchListDTO> getProductListByPurchaseId(Integer purchaseId) {
        return commonMapper.getProductListByPurchaseId(purchaseId);
    }

    /**
     * 根据销售订单id查询明细里的产品列表
     *
     * @param saleId
     * @return
     */
    @Override
    public List<ErpProductAndBatchListDTO> getProductListBySaleId(Integer saleId) {
        return commonMapper.getProductListBySaleId(saleId);
    }

    /**
     * 根据个数在code前面用0补齐
     */
    @Override
    public String getCode(String code, Integer length) {
        StringBuilder sb = new StringBuilder();
        int codeLen = code.length();
        if (codeLen < length) {
            int i1 = length - codeLen;
            for (int i = 0; i < i1; i++) {
                sb.append(0);
            }
            sb.append(code);
            return sb.toString();
        } else {
            return code;
        }
    }

    /**
     * 通过redisKey从redis获取value
     * 如果value为空则设置为1
     *
     * @param redisKey
     * @return
     */
    @Override
    public int getRedisCode(String redisKey) {
        String valueString = jedisService.get(redisKey);
        if (StringUtils.isEmpty(valueString)) {
            jedisService.set(redisKey, "1", 0);
            return 1;
        }
        return Integer.valueOf(valueString);
    }

}

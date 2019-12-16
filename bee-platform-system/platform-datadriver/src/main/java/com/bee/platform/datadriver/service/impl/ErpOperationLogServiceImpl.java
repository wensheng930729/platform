package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.ErpOperationLogMapper;
import com.bee.platform.datadriver.dto.ErpOperationLogDTO;
import com.bee.platform.datadriver.entity.ErpOperationLog;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.service.ErpOperationLogService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 操作日志表 服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Service
public class ErpOperationLogServiceImpl extends ServiceImpl<ErpOperationLogMapper, ErpOperationLog> implements ErpOperationLogService {

    @Autowired
    private ErpOperationLogMapper operationLogMapper;

    @Override
    public ResponseResult<List<ErpOperationLogDTO>> getOperationLog(String businessType, Page page, Integer orgId, Integer orderId) {
        Pagination pagination = PageUtils.transFromPage(page);
        ErpOperationLog operationLog = new ErpOperationLog().setBusinessType(businessType)
                .setCompanyId(orgId).setBusinessId(orderId);
        List<ErpOperationLog> operationLogList = operationLogMapper.selectPage(pagination, new EntityWrapper<>(operationLog)
                .orderBy("operate_time", false));
        List<ErpOperationLogDTO> operationLogDTOList = BeanUtils.assemble(ErpOperationLogDTO.class, operationLogList);
        for (ErpOperationLogDTO dto : operationLogDTOList) {
            switch (dto.getBusinessType()) {
                case "purchase":
                    dto.setBusinessType(EnumBusinessType.PURCHASE.getValue());
                    break;
                case "sale":
                    dto.setBusinessType(EnumBusinessType.SALE.getValue());
                    break;
                case "material_stock":
                    dto.setBusinessType(EnumBusinessType.MATERIAL_STOCK.getValue());
                    break;
                case "material_requisition":
                    dto.setBusinessType(EnumBusinessType.MATERIAL_REQUISITION.getValue());
                    break;
                case "product_stock":
                    dto.setBusinessType(EnumBusinessType.PRODUCT_STOCK.getValue());
                    break;
                case "stock_check":
                    dto.setBusinessType(EnumBusinessType.STOCK_CHECK.getValue());
                    break;
                case "product_delivery":
                    dto.setBusinessType(EnumBusinessType.PRODUCT_DELIVERY.getValue());
                    break;
                case "init_stock":
                    dto.setBusinessType(EnumBusinessType.INIT_STOCK.getValue());
                    break;
                case "material_batch":
                    dto.setBusinessType(EnumBusinessType.MATERIAL_BATCH.getValue());
                    break;
                case "product_archive":
                    dto.setBusinessType(EnumBusinessType.PRODUCT_ARCHIVE.getValue());
                    break;
                case "purchase_statement":
                    dto.setBusinessType(EnumBusinessType.PURCHASE_STATEMENT.getValue());
                    break;
                case "sale_statement":
                    dto.setBusinessType(EnumBusinessType.SALE_STATEMENT.getValue());
                    break;
                case "test_report":
                    dto.setBusinessType(EnumBusinessType.TEST_REPORT.getValue());
                    break;
                case "test_type":
                    dto.setBusinessType(EnumBusinessType.TEST_TYPE.getValue());
                    break;
                case "purchase_invoice":
                    dto.setBusinessType(EnumBusinessType.PURCHASE_INVOICE.getValue());
                    break;
                case "sale_invoice":
                    dto.setBusinessType(EnumBusinessType.SALE_INVOICE.getValue());
                    break;
                case "product_category":
                    dto.setBusinessType(EnumBusinessType.PRODUCT_CATEGORY.getValue());
                    break;
                case "furnace":
                    dto.setBusinessType(EnumBusinessType.FURNACE.getValue());
                    break;
                case "auxiliary_material_consumption":
                    dto.setBusinessType(EnumBusinessType.AUXILIARY_MATERIAL_CONSUMPTION.getValue());
                    break;
                case "customer_profile":
                    dto.setBusinessType(EnumBusinessType.CUSTOMER_PROFILE.getValue());
                    break;
                case "customer_category":
                    dto.setBusinessType(EnumBusinessType.CUSTOMER_CATEGORY.getValue());
                    break;
                case "purchase_payment":
                    dto.setBusinessType(EnumBusinessType.PURCHASE_PAYMENT.getValue());
                    break;
                case "commercial_opportunity":
                    dto.setBusinessType(EnumBusinessType.COMMERCIAL_OPPORTUNITY.getValue());
                    break;
                default:
                    dto.setBusinessType("");
            }

        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, operationLogDTOList, PageUtils.transToPage(pagination));
    }

}

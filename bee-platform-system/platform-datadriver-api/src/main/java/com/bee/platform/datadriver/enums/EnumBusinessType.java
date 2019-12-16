package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 业务类型枚举
 *
 * @author Raphael.dq
 * @date 2019/05/29
 */
@Getter
@AllArgsConstructor
public enum EnumBusinessType {
    /**
     * 采购订单
     */
    PURCHASE("purchase", "采购订单"),
    /**
     * 销售订单
     */
    SALE("sale", "销售订单"),

    /**
     * 原料入库，采购收货
     */
    MATERIAL_STOCK("material_stock", "原料入库"),

    /**
     * 领料出库
     */
    MATERIAL_REQUISITION("material_requisition", "领料出库"),
    /**
     * 成品入库
     */
    PRODUCT_STOCK("product_stock", "成品入库"),
    /**
     * 库存盘点
     */
    STOCK_CHECK("stock_check", "库存盘点"),

    /**
     * 成品出库，销售发货
     */
    PRODUCT_DELIVERY("product_delivery", "成品出库"),
    /**
     * 期初库存
     */
    INIT_STOCK("init_stock", "期初库存"),
    /**
     * 料批
     */
    MATERIAL_BATCH("material_batch", "料批"),
    /**
     * 产品档案
     */
    PRODUCT_ARCHIVE("product_archive", "产品档案"),
    /**
     * 采购结算
     */
    PURCHASE_STATEMENT("purchase_statement", "采购结算"),
    /**
     * 销售结算
     */
    SALE_STATEMENT("sale_statement", "销售结算"),
    /**
     * 销售收款
     */
    SALE_RECEIPT("sale_receipt", "销售收款"),
    /**
     * 化验单
     */
    TEST_REPORT("test_report", "化验单"),
    /**
     * 化验类型
     */
    TEST_TYPE("test_type", "化验类型"),
    /**
     * 采购发票
     */
    PURCHASE_INVOICE("purchase_invoice", "采购发票"),
    /**
     * 采购发票
     */
    SALE_INVOICE("sale_invoice", "销售发票"),
    /**
     * 产品分类
     */
    PRODUCT_CATEGORY("product_category", "产品分类"),
    /**
     * 炉号档案
     */
    FURNACE("furnace", "设备档案"),
    /**
     * 辅材消耗
     */
    AUXILIARY_MATERIAL_CONSUMPTION("auxiliary_material_consumption", "辅材消耗"),
    /**
     * 客户档案
     */
    CUSTOMER_PROFILE("customer_profile", "客户档案"),
    /**
     * 客户分类
     */
    CUSTOMER_CATEGORY("customer_category", "客户分类"),
    /**
     * 采购付款
     */
    PURCHASE_PAYMENT("purchase_payment", "采购付款"),
    /**
     * CRM商机客户
     */
    COMMERCIAL_OPPORTUNITY("commercial_opportunity", "CRM商机客户"),
    ;

    private String code;
    private String value;

}

package com.bee.platform.dinas.datadriver.enums;

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
    PURCHASE("dinas_purchase", "采购订单"),
    /**
     * 销售合同
     */
    SALE("sale", "销售合同"),
    
    /**
     * 采购发票
     */
    DINASPURCHASEINVOICE("dinasPurchaseInvoice", "采购发票"),
    
    /**
     * 采购付款
     */
    DINASPURCHASEPAY("dinasPurchasePay", "采购发票"),

    /**
     * 销售回款
     */
    SALE_PAYMENT("sale", "销售合同"),
    /**
     * 验货磅单
     */
    INSPECTION_GOODS("InspectionGoods", "验货磅单"),
    /**
     * 采购结算
     */
    PURCHASE_SETTLEMENT("purchaseSettlement","采购结算"),

    /**
     * 销售结算
     */
    SALE_SETTLEMENT("saleSettlement","销售结算"),

    /**
     * 销售发票
     */
    SALE_INVOICE("saleSettlement","销售发票"),
    /**
     * 产品
     */
    PRODUCT("product","产品"),
    /**
     * 供货商
     */
    SUPPLY_CUSTOMER("supply_customer","供货商"),
    /**
     * 订货商
     */
    ORDER_CUSTOMER("order_customer","订货商"),
    ;

    private String code;
    private String value;

}

package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName EnumOrderType
 * @Description 功能描述
 * @Date 2019/6/2 10:36
 **/
@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum  EnumOrderType {


    ERP_PURCHASE_ORDER(0,"采购订单"),
    ERP_PURCHASING_AND_RECEIVING(1,"采购收货"),
    ERP_PURCHASING_settlement(2,"采购结算"),
    ERP_PURCHASING_payment(3,"采购付款"),
    ERP_SALE_ORDER(4,"销售订单"),
    ERP_SALE_SHIPMENT(5,"销售发货"),
    ERP_SALE_SETTLEMENT(6,"销售结算"),
    ERP_SALE_RECEIVABLE(7,"销售收款"),
    ERP_OPENING_INVENTORY(8,"期初单号"),
    ERP_OUT_OF_STOCK(9,"领料出库"),
    ERP_WAREHOUSING(10,"成品入库"),



    ;

    private Integer key;
    private String value;



}

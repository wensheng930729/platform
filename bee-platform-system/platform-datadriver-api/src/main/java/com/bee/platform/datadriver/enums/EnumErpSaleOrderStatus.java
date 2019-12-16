package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @Classname EnumErpSaleOrderStatus
 * @Description 销售订单各种状态
 * @Date 2019/6/11 16:36
 * @Author xin.huang
 */
public class EnumErpSaleOrderStatus {
    /**
     * @Description 发货状态
     */
    @Getter
    @AllArgsConstructor
    @NoArgsConstructor
    public enum DeliveryType {

        N0T_DELIVERY(0, "未发货"),
        PART_DELIVERY(1, "部分发货"),
        ALL_DELIVERY(2, "全部发货");

        private int key;
        private String val;

    }

    /**
     * @Description 开票状态
     */
    @Getter
    @AllArgsConstructor
    @NoArgsConstructor
    public enum InvoiceType {

        N0T_INVOICE(0, "未开票"),
        HAD_INVOICE(1, "已开票");

        private int key;
        private String val;

    }

    /**
     * @Description 结算状态
     */
    @Getter
    @AllArgsConstructor
    @NoArgsConstructor
    public enum AccountType {

        N0T_ACCOUNTT(0, "未结算"),
        HAD_ACCOUNTT(1, "已结算");

        private int key;
        private String val;

    }

}

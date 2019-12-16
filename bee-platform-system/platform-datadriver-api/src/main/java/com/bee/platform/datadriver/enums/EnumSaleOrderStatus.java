package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @Classname EnumSaleOrderStatus
 * @Description 销售订单状态
 * @Date 2019/6/10 19:25
 * @Author xin.huang
 */
@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum  EnumSaleOrderStatus {
    SIGNED(0,"已签订"),

    EXECUTION (1,"执行中"),

    DELIVERY_COMPLETED(2,"发货完成/收货完成"),

    SETTLED(3,"已结算"),

    RECEIVED(4,"已收款/已付款");

    private Integer key;
    private String value;
}

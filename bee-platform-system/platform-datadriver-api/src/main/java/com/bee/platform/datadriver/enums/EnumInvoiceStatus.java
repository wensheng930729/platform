package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumInvoiceStatus
 * @Description 发票状态枚举
 * @author jie.chen
 * @Date 2019/5/29$ 17:46$
 * @version 1.0.0
 */


@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum EnumInvoiceStatus {

    /**
     * 未开票
     */
    INVOICE_NOT(0,"未开票"),

    /**
     * 已开票
     */
    INVOICE_ALREADY(1,"已开票");

    private Integer key;
    private String value;
}

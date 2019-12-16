package com.bee.platform.customer.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumIsManager
 * @Description erp客户一级分类枚举
 */

public class EnumCustomerCategory {
    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum first{

        supply("1","供应"),
        sale("2","销售"),
        produce("3","生产"),
        Operate("4","运营"),
        government("5","政府"),
        ;

        private String code;
        private String name;

    }
}

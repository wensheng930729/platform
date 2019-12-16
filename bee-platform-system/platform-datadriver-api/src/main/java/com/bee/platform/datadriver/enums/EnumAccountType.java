package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumAccountStatus
 * @Description 结算类型
 * @author jie.chen
 * @Date 2019/5/29$ 17:44$
 * @version 1.0.0
 */

@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum EnumAccountType {
    /**
     * 未结算
     */
    ACCOUNT_NOT(0,"未结算"),

    /**
     * 已结算
     */
    ACCOUNT_ALREADY(1,"已结算");

    private Integer key;
    private String value;
}

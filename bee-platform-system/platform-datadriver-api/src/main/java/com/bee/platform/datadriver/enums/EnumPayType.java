package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumPayType
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/31$ 10:33$
 * @version 1.0.0
 */

@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum EnumPayType {

    PAY_NOT(0,"未付款/未收款"),

    PAY_ALREADY(1,"已付款/已收款");

    private Integer key;
    private String value;
}

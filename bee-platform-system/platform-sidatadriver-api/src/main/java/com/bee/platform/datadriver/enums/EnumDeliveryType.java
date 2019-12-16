package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumDeliveryType
 * @Description 发货状态
 * @author jie.chen
 * @Date 2019/5/31$ 10:29$
 * @version 1.0.0
 */
@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum EnumDeliveryType {

    DELIVERY_NOT(0,"未发货"),

    DELIVERY_ALL(1,"部分发货"),

    DELIVERY_PART(2,"全部发货");
    private Integer key;
    private String value;
}

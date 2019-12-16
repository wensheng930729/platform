package com.bee.platform.datadriver.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @description: 收货状态
 * @author: junyang.li
 * @create: 2019-03-20 15:13
 **/
@Getter
@AllArgsConstructor
@NoArgsConstructor
public enum EnumReceiveType {

    RECEIVE_NOT(0,"未收货/未发货"),

    RECEIVED_PART(1,"部分收货/部分发货"),

    RECEIVED_ALL(2,"全部收货/全部发货");

    private Integer key;
    private String value;
}

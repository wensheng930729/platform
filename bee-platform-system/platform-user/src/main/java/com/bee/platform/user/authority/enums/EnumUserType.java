 package com.bee.platform.user.authority.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Raphael.dq
 * @date 2019/06/14
 */
@AllArgsConstructor
@Getter
public enum EnumUserType {

    CONSOLE_USER(1, "后台用户"), CUSTOMER(0, "中台用户");
    
    private Integer code;
    private String description;
}

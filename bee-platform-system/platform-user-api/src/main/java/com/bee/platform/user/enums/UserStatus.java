package com.bee.platform.user.enums;

import lombok.Getter;

/**
 * @description: 用户状态
 * @author: junyang.li
 * @create: 2019-03-20 15:13
 **/
@Getter
public enum  UserStatus {
    /**
     *  用户状态
     */
    UNUSER(0,"已禁用"),
    USEING(1,"启用中");

    private Integer code;

    private String desc;

    UserStatus(){

    }

    UserStatus(Integer code, String desc) {
        this.code = code;
        this.desc = desc;
    }
}

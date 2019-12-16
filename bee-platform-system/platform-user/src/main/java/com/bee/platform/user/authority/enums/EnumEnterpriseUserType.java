package com.bee.platform.user.authority.enums;

import lombok.Getter;

/**
 * @Classname EnumEnterpriseUserType
 * @Description 用户角色类型
 * @Date 2019/6/3 15:04
 * @Author xin.huang
 */
@Getter
public enum  EnumEnterpriseUserType {
    /**
     *  用户账号类型
     */
    ORDINARY_USER(1,"普通用户"),
    ENTERPRISE_ADMIN(2,"管理员");

    private Integer code;

    private String desc;

    EnumEnterpriseUserType(){

    }

    EnumEnterpriseUserType(Integer code, String desc) {
        this.code = code;
        this.desc = desc;
    }
}

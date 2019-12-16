package com.bee.platform.user.authority.enums;

import lombok.Getter;

/**
 * @Classname RoleType
 * @Description 角色类型
 * @Date 2019/5/27 20:00
 * @Author xin.huang
 */
@Getter
public enum AuthRoleType {
    /**
     * 角色
     */
    ENTERPRISE_MEMBER(0, "enterprise_member"),ENTERPRISE_ADMIN(1,"enterprise_admin"),SUPER_ADMIN(2,"super_admin");

    private Integer code;

    private String desc;

    AuthRoleType(){

    }


    AuthRoleType(Integer code, String desc){
        this.code=code;
        this.desc=desc;
    }
}

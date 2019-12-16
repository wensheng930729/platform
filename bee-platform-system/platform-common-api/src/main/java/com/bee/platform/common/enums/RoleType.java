package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description:
 * @author: junyang.li
 * @create: 2018-12-19 14:42
 **/
@Getter
public enum RoleType {

    /**
     * 角色
     */
    SUPER(1,"super"),ADMIN(2,"admin"),USER(3,"user"),MANAGER(4,"manager");

    private Integer code;

    private String desc;

    RoleType(){

    }


    RoleType(Integer code,String desc){
        this.code=code;
        this.desc=desc;
    }

    /**
     * @notes: 判断是否是管理员
     * @Author: junyang.li
     * @Date: 17:22 2019/5/15
     * @return: int 0 false ，1  true
     */
    public static   int isManage(Integer roleId){
        if(roleId==null){
            return Status.FALSE.getKey();
        }
        return SUPER.getCode().equals(roleId)?Status.TRUE.getKey():Status.FALSE.getKey();
    }
}

package com.bee.platform.user.enums;

import lombok.Getter;

import java.util.Arrays;

/**
 * @Description 用户账号类型
 * @Author xin.huang
 * @Date 2019/4/26 15:43
 * @Version 1.0.0
 */
@Getter
public enum AccountType {
    /**
     *  用户账号类型
     */
    phone(0,"手机号"),
    email(1,"邮箱");

    private Integer code;

    private String desc;

    AccountType(){

    }

    AccountType(Integer code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    /**
     * @notes: 根据id查询账号类型
     * @Author: junyang.li
     * @Date: 14:06 2019/5/15
     * @param code :
     * @return: com.bee.platform.user.enums.AccountType
     */
    public static AccountType getType(Integer code){
        if(code==null){
            return null;
        }
       return  Arrays.stream(AccountType.values()).filter(obj->obj.getCode().equals(code)).findFirst().get();
    }
}

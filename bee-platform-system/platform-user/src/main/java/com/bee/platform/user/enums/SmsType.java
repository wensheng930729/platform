package com.bee.platform.user.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @description:  短信类型
 * @author: junyang.li
 * @create: 2019-03-05 14:54
 **/
@Getter
@NoArgsConstructor
public enum  SmsType {
    /**
     * 短信类型
     */
    VERIFICATION_CODE(1,"验证码"),INVITATION_CODE(2,"邀请码");

    private Integer key;

    private String value;

    SmsType(Integer key,String value){
        this.key=key;
        this.value=value;
    }

}

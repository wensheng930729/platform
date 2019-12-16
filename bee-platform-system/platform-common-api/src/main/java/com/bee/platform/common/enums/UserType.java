package com.bee.platform.common.enums;

/**
 * @Classname UserType
 * @Description 用户类型
 * @Date 2019/5/24 10:03
 * @Author xin.huang
 */

import lombok.Getter;
import lombok.NoArgsConstructor;
@Getter
@NoArgsConstructor
public enum UserType {
    /**
     * 中台用户
     */
    MIDDLE_USER(0),
    /**
     * 后台用户
     */
    BACKGROUND_USER(1),
    /**
     * 普通用户
     */
    ORDINARY_USER(2),
    /**
     * 中台和后台用户
     */
    MIDDLE_BACKGROUND_USER(3);

    private Integer key;

    UserType(Integer key){
        this.key=key;
    }
}

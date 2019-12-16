 package com.bee.platform.user.authority.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
  * 用户激活类型
 * @author Raphael.dq
 * @date 2019/06/14
 */
@AllArgsConstructor
@Getter
public enum EnumActiveType {
     REGISTER(0, "平台注册"), ADD(1,"后台添加");
    
    private Integer code;
    private String description;
}

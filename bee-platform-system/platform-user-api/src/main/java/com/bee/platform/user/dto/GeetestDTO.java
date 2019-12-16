package com.bee.platform.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-04-28 16:02
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class GeetestDTO implements Serializable {

    private static final long serialVersionUID = -5547079712152030224L;
    /**
     * 返回状态码
     */
    private Integer success;
    /**
     * 验证事件流水号
     */
    private String challenge;
    /**
     * 公钥
     */
    private String gt;
    /**
     * 客户端类型
     */
    private String clientType;
}

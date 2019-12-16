package com.bee.platform.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * Created by CrazyMouse on 2016/11/2.
 * 用户表
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthUserBoxDTO implements Serializable{

    private static final long serialVersionUID = -8321972049304499627L;
    /**
     * 用户id
     */
    private Integer id;
    /**
     * 名称
     */
    private String name;

}

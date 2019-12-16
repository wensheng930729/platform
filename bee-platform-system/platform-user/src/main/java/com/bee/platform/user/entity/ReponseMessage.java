package com.bee.platform.user.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * Created by CrazyMouse on 2016/11/2.
 */
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
public class ReponseMessage implements Serializable {
    private int code;
    private String message;
    private Object object;
}

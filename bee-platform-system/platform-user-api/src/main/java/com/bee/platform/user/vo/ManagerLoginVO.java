package com.bee.platform.user.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * @description:  管理后台成员登录参数
 * @author: junyang.li
 * @create: 2019-04-29 08:58
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
@ApiModel("管理后台成员登录参数包含极验参数")
public class ManagerLoginVO implements Serializable {

    private static final long serialVersionUID = -6207928951247968237L;

    @ApiModelProperty(value = "账号",required = true)
    @NotEmpty(message = "账号不能为空")
    private String username;

    @ApiModelProperty(value = "密码",required = true)
    @NotEmpty(message = "密码不能为空")
    private String password;

    /*@ApiModelProperty(value = "极验验证事件流水号",required = true)
    @NotEmpty(message = "极验验证事件流水号不能为空")
    private String challenge;

    @ApiModelProperty(value = "极验公钥",required = true)
    @NotEmpty(message = "极验公钥不能为空")
    private String validate;

    @ApiModelProperty(value = "极验seccode",required = true)
    @NotEmpty(message = "极验seccode不能为空")
    private String seccode;*/
}

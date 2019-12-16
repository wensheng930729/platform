package com.bee.platform.user.authority.rq;

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
 * @Classname AuthManagerRQ
 * @Description 后台管理员登录请求信息
 * @Date 2019/6/1 13:31
 * @Author xin.huang
 */
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
@ApiModel("平台用户登录请求信息")
public class AuthUserRQ implements Serializable {
    private static final long serialVersionUID = 5627192141015408526L;

    @ApiModelProperty(value = "账号",required = true)
    @NotEmpty(message = "账号不能为空")
    private String username;

    @ApiModelProperty(value = "密码",required = true)
    @NotEmpty(message = "密码不能为空")
    private String password;

    @ApiModelProperty(value = "用户类型",required = true)
    private Integer userType = 0;
}

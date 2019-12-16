package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.validation.constraints.NotEmpty;

/**
 * @Classname RegisterUserRQ
 * @Description 注册传入参数
 * @Date 2019/5/24 11:44
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel("注册最后一步传入参数")
public class RegisterUserRQ {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("姓名")
    @NotEmpty(message = "姓名不能为空")
    private String name;

    @ApiModelProperty("密码")
    @NotEmpty(message = "密码不能为空")
    private String password;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("手机号")
    @NotEmpty(message = "手机号不能为空")
    private String phone;
}

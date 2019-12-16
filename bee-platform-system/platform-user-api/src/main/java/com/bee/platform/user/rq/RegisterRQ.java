package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * @description: 注册传入参数
 * @author: junyang.li
 * @create: 2019-03-04 14:34
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel("注册最后一步传入参数")
public class RegisterRQ implements Serializable {

    private static final long serialVersionUID = 2912221320295224117L;

    @ApiModelProperty("昵称")
    @NotEmpty(message = "昵称不能为空")
    private String nickname;

    @ApiModelProperty("密码")
    @NotEmpty(message = "密码不能为空")
    private String password;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("手机号")
    @NotEmpty(message = "手机号不能为空")
    private String phone;
}

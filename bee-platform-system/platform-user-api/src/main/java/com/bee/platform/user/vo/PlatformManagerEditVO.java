package com.bee.platform.user.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import java.io.Serializable;

/**
 * @description: 个人中心修改个人信息
 * @author: junyang.li
 * @create: 2019-05-05 16:13
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("个人中心修改个人信息")
public class PlatformManagerEditVO implements Serializable {

    private static final long serialVersionUID = -7093295684922014257L;

    @ApiModelProperty(value = "手机号",required = true)
    @NotEmpty(message = "手机号不能为空")
    @Length(max = 11,message = "手机号限制11个字符")
    @Pattern(regexp = "^(1)\\d{10}$",message = "手机号不正确")
    private String username;

    @ApiModelProperty(value = "手机验证码",required = true)
    @Length(max = 6,message = "手机号限制6个字符")
    private String phoneCode;

    @ApiModelProperty(value = "邮箱",required = true)
    @Length(max = 50,message = "邮箱限制120个字符")
    private String email;

    @ApiModelProperty(value = "邮箱验证码",required = true)
    @Length(max = 6,message = "邮箱验证码限制6个字符")
    private String emailCode;

    @ApiModelProperty(value = "昵称",required = true)
    @NotBlank(message = "账户名不能为空")
    @Length(max = 16,message = "账户名限制16个字符")
    private String nickname;

    @ApiModelProperty(value = "账户说明",required = true)
    @Length(max = 100,message = "账户说明限制120个字符")
    private String notes;

    @ApiModelProperty(value = "头像",required = true)
    @Length(max = 100,message = "头像地址长度限制100个字符")
    private String head;
}

package com.bee.platform.user.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

/**
 * @description: 管理后台添加用户传入参数
 * @author: junyang.li
 * @create: 2019-04-25 13:41
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("管理后台添加用户传入参数")
public class PlatformManagerVO {

    @ApiModelProperty(value = "用户id")
    private Integer managerId;

    @ApiModelProperty(value = "手机号",required = true)
    @NotEmpty(message = "手机号不能为空")
    @Length(max = 11,message = "手机号限制11个字符")
    @Pattern(regexp = "^(1)\\d{10}$",message = "手机号不正确")
    private String username;

    /**
     * 第一版暂时不做短信验证码校验，第二版需要做，所有暂时不删代码
     */
    /*@ApiModelProperty(value = "手机验证码",required = true)
    @Length(max = 6,message = "手机号限制6个字符")
    private String phoneCode;*/

    @ApiModelProperty(value = "邮箱")
    @Length(max = 50,message = "邮箱限制120个字符")
    private String email;

    @ApiModelProperty(value = "昵称",required = true)
    @NotBlank(message = "账户名不能为空")
    @Length(max = 16,message = "账户名限制16个字符")
    private String nickname;

    @ApiModelProperty(value = "角色id",required = true)
    @NotNull(message="权限组不能为空")
    @Range(message = "权限组编号不能为负数")
    private Integer roleId;

    @ApiModelProperty(value = "账户说明")
    @Length(max = 100,message = "账户说明限制120个字符")
    private String notes;

    @ApiModelProperty(value = "头像")
    @Length(max = 100,message = "头像地址长度限制100个字符")
    private String head;
}

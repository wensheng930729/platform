package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotNull;
import java.io.Serializable;


/**
 * @ClassName EditUserPhoneEmailRQ
 * @Description 修改用户账号信息
 * @Author xin.huang
 * @Date 2019/4/26 14:54
 * @Version 1.0.0
 */
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("修改用户账号信息")
public class EditUserAccountRQ implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户ID")
    @NotNull(message = "用户ID不能为空")
    private  Integer id;

    @ApiModelProperty("邮箱")
    private  String email;

    @ApiModelProperty("手机号")
    private  String username;

    @ApiModelProperty("账号类型：0手机号，1邮箱")
    @Range(max = 1L, message = "暂时只有手机号，邮箱")
    private Integer type;

}

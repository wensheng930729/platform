package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;


@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("权限-用户管理列表返回数据")
public class ManagerUsersDTO implements Serializable {

    private static final long serialVersionUID = -8634947405482834886L;

    @ApiModelProperty("用户id")
    private Integer managerId;

    @ApiModelProperty("账户名")
    private String nickname;

    @ApiModelProperty("phone/email")
    private String phoneEmail;

    @ApiModelProperty("权限组名称")
    private String roleName;

    @ApiModelProperty("注册时间")
    private Date createAt;

    @ApiModelProperty("账户状态")
    private Integer status;
}

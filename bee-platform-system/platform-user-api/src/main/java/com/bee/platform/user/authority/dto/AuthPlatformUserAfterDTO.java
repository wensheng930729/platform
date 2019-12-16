package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("后台用户查询返回对象")
public class AuthPlatformUserAfterDTO implements Serializable {
    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("手机号")
    private String phone;

    @ApiModelProperty("姓名")
    private String name;

    @ApiModelProperty("用户账号")
    private String username;

    @ApiModelProperty("邮箱")
    private String email;
    
    @ApiModelProperty("角色类型")
    private String roleName;

    @ApiModelProperty("是否启用：1启用 0禁用")
    private Integer status;

    @ApiModelProperty("创建时间")
    private Date createTime;

}

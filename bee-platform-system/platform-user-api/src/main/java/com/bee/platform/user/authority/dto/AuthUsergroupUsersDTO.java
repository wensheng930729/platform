package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "用户组下所有用户DTO")
public class AuthUsergroupUsersDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("用户名")
    private String username;

    @ApiModelProperty("公司名称")
    private String enterprise;

    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("部门名称")
    private String department;

    @ApiModelProperty("职位名称")
    private String post;


}

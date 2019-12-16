package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 用户管理查询用户的部门信息
 * @author: junyang.li
 * @create: 2019-03-20 14:56
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("用户管理查询用户的部门信息")
public class UserDepartmentDTO implements Serializable {

    private static final long serialVersionUID = 4679737975937525535L;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("部门")
    private String department;
}

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
 * @description: 用户管理列表查询返回数据
 * @author: junyang.li
 * @create: 2019-03-20 14:06
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("用户管理列表查询返回数据")
public class UserManagementDTO implements Serializable {

    private static final long serialVersionUID = 6786747450490079629L;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("用户账号")
    private String username;

    @ApiModelProperty("用户姓名")
    private String nickname;

    @ApiModelProperty("用户职位")
    private String position;

    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("部门名称")
    private String department;

    @ApiModelProperty("用户状态")
    private Integer userStatus;

    @ApiModelProperty("用户状态详细")
    private String userStatusDesc;
}

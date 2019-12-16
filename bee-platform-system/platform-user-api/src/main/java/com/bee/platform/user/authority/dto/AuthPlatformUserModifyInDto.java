package com.bee.platform.user.authority.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "判断中平台用户返回数据")
public class AuthPlatformUserModifyInDto implements Serializable{
	@ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("归属公司id")
    private Integer enterpriseId;

    @ApiModelProperty("归属公司名称")
    private String enterpriseName;

    @ApiModelProperty("归属公司简称")
    private String enterpriseSimpleName;

    @ApiModelProperty("部门Id")
    private Integer departmentId;

    @ApiModelProperty("部门名称")
    private String departmentName;

    @ApiModelProperty("职位Id")
    private Integer postId;

    @ApiModelProperty("职位名称")
    private String postName;

    @ApiModelProperty("姓名")
    private String name;

    @ApiModelProperty("用户账号")
    private String username;

    @ApiModelProperty("用户名")
    private String nickname;

    @ApiModelProperty("是否启用：1启用 0禁用")
    private Integer status;
    
    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("创建时间")
    private String createTime;
}

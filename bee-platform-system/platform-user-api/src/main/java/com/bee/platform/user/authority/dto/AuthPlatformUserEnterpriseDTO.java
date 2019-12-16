package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "用户企业相关的")
public class AuthPlatformUserEnterpriseDTO implements Serializable{

    @ApiModelProperty("主键id")
    private Integer id;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    @ApiModelProperty("部门id")
    private Integer departmentsId;

    @ApiModelProperty("职位id")
    private Integer postId;

    @ApiModelProperty("状态：1启动 0禁用")
    private Integer status;

    @ApiModelProperty("是否删除 0未删除 1删除")
    private Integer deleted;

    @ApiModelProperty("创建人")
    private Integer createUser;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("更新时间")
    private Date updateTime;

}

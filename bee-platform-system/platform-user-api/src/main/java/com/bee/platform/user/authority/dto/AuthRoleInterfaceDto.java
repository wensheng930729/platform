package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @Classname AuthRoleInterfaceDto
 * @Description 角色下的接口返回数据
 * @Date 2019/5/21 15:32
 * @Author xin.huang
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "角色下的接口返回数据")
public class AuthRoleInterfaceDto implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("角色id")
    private Integer roleId;

    @ApiModelProperty("接口id")
    private Integer interfaceId;

    @ApiModelProperty("是否启用：1启用 0禁用")
    private Integer status;

    @ApiModelProperty("该角色下接口序号")
    private Integer orderNumId;

    @ApiModelProperty("创建人ID")
    private Integer createUser;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("更新时间")
    private Date updateTime;
}

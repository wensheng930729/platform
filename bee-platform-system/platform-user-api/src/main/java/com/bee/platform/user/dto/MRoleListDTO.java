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

/**
 * @description: 后台角色组列表
 * @author: junyang.li
 * @create: 2019-04-29 16:26
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel(value = "后台角色组列表查询数据")
public class MRoleListDTO implements Serializable {

    private static final long serialVersionUID = -1748859063165804160L;
    @ApiModelProperty("角色id")
    private Integer roleId;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("账户数量")
    private Integer accountNum;

    @ApiModelProperty("修改人id")
    private Integer modifyId;

    @ApiModelProperty("修改人姓名")
    private String modifier;

    @ApiModelProperty("修改时间")
    private Date modifyTime;
}

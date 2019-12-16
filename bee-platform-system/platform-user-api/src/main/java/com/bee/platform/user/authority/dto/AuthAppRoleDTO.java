package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName AuthRoleDTO
 * @Description 角色详细信息
 * @author jie.chen
 * @Date 2019/5/20$ 14:37$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("应用角色信息-中台面板")
public class AuthAppRoleDTO implements Serializable {

    private static final long serialVersionUID = -6345474790320680856L;

    @ApiModelProperty("角色id")
    private Integer id;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("应用缩写")
    private String abbreviation;

    @ApiModelProperty("工业大脑子系统标识（1BI 2录入）")
    private Integer brainSys;
}

package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 企业与角色（角色或功能的关联表）的中间表
 * </p>
 *
 * @author LILIANG
 * @since 2019-06-03
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "查询企业下的所有应用和功能DTO")
public class AuthEnterpriseAppFunDTO implements Serializable{

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @ApiModelProperty("主键id")
    private Integer id;
    /**
     * pid
     */
    @ApiModelProperty("pid")
    private Integer pid;
    /**
     * 角色名称
     */
    @ApiModelProperty("角色名称")
    private String roleName;
    /**
     * 角色类型
     */
    @ApiModelProperty("角色类型")
    private String roleType;
    /**
     * 角色级别
     */
    @ApiModelProperty("角色级别")
    private Integer level;
    /**
     * 子系统标识
     */
    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("功能")
    private List<AuthEnterpriseAppFunDTO> children;
}

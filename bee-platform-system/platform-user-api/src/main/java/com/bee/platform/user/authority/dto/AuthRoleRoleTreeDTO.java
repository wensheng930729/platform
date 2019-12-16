package com.bee.platform.user.authority.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author cheng.ke
 * @version 1.0.0
 * @ClassName AuthUserRoleTreeDTO
 * @Description 功能描述
 * @Date 2019/5/23 17:21
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("应用功能角色树")
@JsonInclude
public class AuthRoleRoleTreeDTO {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("主键id")
    private Integer id;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("该用户下角色或功能的自定义分类，基于level字段的一个分类，用于分类展示")
    private String roleType;

    @ApiModelProperty("该用户下角色级别：与角色表的level对应")
    private Integer level;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("角色父id")
    private Integer pid;

    @ApiModelProperty("子用户角色")
    private List<AuthRoleRoleTreeDTO> children;

}

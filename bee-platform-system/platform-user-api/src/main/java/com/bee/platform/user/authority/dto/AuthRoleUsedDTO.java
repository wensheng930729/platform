package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author liliang
 * @version 1.0.0
 * @ClassName AuthRoleTreeDTO
 * @Description 角色树详细信息
 * @Date 2019/5/24$ 14:37$
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("角色权限配置已有功能DTO")
public class AuthRoleUsedDTO implements Serializable {

    private static final long serialVersionUID = -6345474790320680856L;

    @ApiModelProperty("角色id")
    private Integer id;

    @ApiModelProperty("角色pid")
    private Integer pid;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("角色级别")
    private Integer level;

    @ApiModelProperty("子系统标识")
    private String subSys;

}

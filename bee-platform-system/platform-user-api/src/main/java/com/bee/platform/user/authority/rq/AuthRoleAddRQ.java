package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @ClassName AuthRoleAddRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/20$ 16:59$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("角色添加请求信息")
public class AuthRoleAddRQ implements Serializable {

    private static final long serialVersionUID = 873789829900222707L;

    @ApiModelProperty("角色id")
    private String id;

    @ApiModelProperty("角色名称")
    @NotEmpty
    private String roleName;

    @NotEmpty
    @ApiModelProperty("角色类型")
    private String roleType;

    @NotNull
    @ApiModelProperty("角色级别")
    private Integer level;

    @NotEmpty
    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("下一级角色（应用、功能角色添加使用）")
    private List<AuthRoleAddRQ> roleAddRQS;

    @ApiModelProperty("基础角色对应菜单（基础功能添加使用）")
    private List<AuthResourceRQ> resourceRQS;

    @ApiModelProperty("基础角色对应接口（基础功能添加使用）")
    private List<AuthInterfaceDetailRQ> interfaceDetailRQS;

}

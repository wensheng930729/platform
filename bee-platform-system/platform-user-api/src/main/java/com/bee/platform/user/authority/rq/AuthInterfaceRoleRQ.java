package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @Classname AuthInterfaceRoleRQ
 * @Description 接口添加角色传输对象
 * @Date 2019/5/21 14:23
 * @Author xin.huang
 */
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("接口添加角色传输对象")
public class AuthInterfaceRoleRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("角色ID")
    @NotNull(message = "角色ID不能为空")
    private Integer roleId;

    @ApiModelProperty("接口ID")
    @NotNull(message = "接口ID不能为空")
    private Integer interfaceId;

    @ApiModelProperty("该角色下的接口序号")
    private Integer orderNum;

    @ApiModelProperty("创建人ID")
    @NotNull(message = "创建人ID不能为空")
    private Integer createUser;

}

package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author cheng.ke
 * @version 1.0.0
 * @ClassName UserRelationRoleRQ
 * @Description 功能描述
 * @Date 2019/5/21 11:51
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业用户关联角色功能应用请求参数")
public class UserRelationRoleRQ implements Serializable {

	private static final long serialVersionUID = 9201784476170148394L;

    @ApiModelProperty("企业id")
    @NotNull(message = "企业id不能为空")
    private Integer enterpriseId;

    @ApiModelProperty("用户id")
    @NotNull(message = "用户id不能为空")
    private Integer userId;

    @ApiModelProperty("角色功能应用id")
    @NotNull(message = "角色功能应用id不能为空")
    private Integer roleId;

    @ApiModelProperty("角色父id")
    @NotNull(message = "角色父id不能为空")
    private Integer pid;

    @ApiModelProperty("角色级别")
    @NotNull(message = "角色级别不能为空")
    private Integer level;

    @ApiModelProperty("角色类型")
    @NotEmpty(message = "角色类型不能为空")
    private String roleType;

    @ApiModelProperty("标识 1勾选 0未勾选")
    @NotNull(message = "标识不能为空")
    private Integer flag;

    @ApiModelProperty("创建人id")
    @NotNull(message = "创建人id不能为空")
    private Integer createUser;



}

package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "用户组下所有用户列表RQ")
public class AuthUsergroupUsersListRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户组id")
    @NotNull(message = "用户组id不能为空")
    private Integer groupId;

    @ApiModelProperty("部门")
    private Integer deparmentId;

    @ApiModelProperty("用户名")
    private String username;

    private Integer enterpriseId;

}

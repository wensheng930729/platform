package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName AssignPermissionRQ
 * @Description 功能描述
 * @Date 2019/6/13 9:41
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("给用户组授权请求参数")
public class AssignPermissionToUsergroupRQ implements Serializable {
    private static final long serialVersionUID = 9201784476170148394L;

    @ApiModelProperty("用户组id")
    @NotNull(message = "用户组id不能为空")
    private Integer usergroupId;

    @ApiModelProperty("企业id")
    @NotNull(message = "企业id不能为空")
    private Integer enterpriseId;

    @ApiModelProperty("用户角色关系集合")
    private List<UsergroupRelationRoleRQ> usergroupRoleList;



}

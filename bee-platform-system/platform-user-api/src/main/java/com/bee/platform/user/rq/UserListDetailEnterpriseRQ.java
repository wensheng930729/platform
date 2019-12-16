package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author liliang
 * @since 2019-04-28
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户管理列表关联企业详情修改入参")
public class UserListDetailEnterpriseRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    @NotNull(message = "企业id不能为空")
    private Integer id;

    @ApiModelProperty("是否启用")
    @NotNull(message = "是否启用不能为空")
    private Integer isActive;

    @ApiModelProperty("是否管理员")
    @NotNull(message = "是否管理员id不能为空")
    private Integer isManager;

/*    @ApiModelProperty("所属部门id")
    @NotNull(message = "所属部门id不能为空")
    private Integer departmentId;

    @ApiModelProperty("所属部门旧id")
    @NotNull(message = "所属部门旧id不能为空")
    private Integer oldDepartmentId;

    @ApiModelProperty("职位id")
    @NotNull(message = "职位id不能为空")
    private Integer postId;

    @ApiModelProperty("职位旧id")
    @NotNull(message = "职位旧id不能为空")
    private Integer oldPostId;*/

}

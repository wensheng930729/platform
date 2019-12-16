package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * @author cheng.ke
 * @version 1.0.0
 * @ClassName RoleRelationRoleRQ
 * @Description 角色关联功能 功能关联应用请求参数
 * @Date 2019/5/20 17:14
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("角色关联功能 功能关联应用请求参数")
public class RoleRelationRoleRQ {

    @ApiModelProperty("子id集合")
    @NotEmpty(message = "子id集合不能为空")
    private List<Integer> childIds;

    @ApiModelProperty("父id")
    @NotNull(message = "父id不能为空")
    private Integer parentId;

    @ApiModelProperty("业务类型,0添加，1修改，2删除")
    @NotNull(message = "业务类型不能为空")
    @Min(value = 0,message = "只能为0，1，2")
    @Max(value = 2,message = "只能为0，1，2")
    private Integer type;



}

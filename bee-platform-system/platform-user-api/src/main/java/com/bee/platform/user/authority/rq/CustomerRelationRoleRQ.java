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
 * @ClassName CustomerRelationRoleRQ
 * @Description 功能描述
 * @Date 2019/5/21 11:51
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("客户关联角色/功能/应用请求参数")
public class CustomerRelationRoleRQ {
    @ApiModelProperty("角色功能应用id集合")
    @NotEmpty(message = "角色功能应用id集合不能为空")
    private List<Integer> roleIds;

    @ApiModelProperty("客户id")
    @NotNull(message = "客户id不能为空")
    private Integer customerId;

    @ApiModelProperty("业务类型,0添加，1修改，2删除")
    @NotNull(message = "业务类型不能为空")
    @Min(value = 0,message = "只能为0，1，2")
    @Max(value = 2,message = "只能为0，1，2")
    private Integer type;

}

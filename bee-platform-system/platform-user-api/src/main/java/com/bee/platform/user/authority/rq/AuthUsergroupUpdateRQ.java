package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "用户组更新rq")
public class AuthUsergroupUpdateRQ implements Serializable{

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;

    @ApiModelProperty("用户组名")
    @NotEmpty(message = "用户组名不能为空")
    private String groupName;

    @ApiModelProperty("描述")
    private String description;

    @ApiModelProperty("是否默认 0否 1是")
    @NotNull(message = "是否默认不能为空")
    private Integer defaultValue;


}

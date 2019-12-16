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
 * @ClassName AuthUsergroupRalationUserRQ
 * @Description 功能描述
 * @Date 2019/7/17 15:22
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("用户组关联用户请求参数")
public class AuthUsergroupRalationUserRQ implements Serializable {
    private static final long serialVersionUID = 3629911020012849626L;

    @ApiModelProperty("用户组id")
    @NotNull(message = "用户组id不能为空")
    private Integer usergroupId;

    @ApiModelProperty("用户id集合")
    private List<Integer> userIds;

}

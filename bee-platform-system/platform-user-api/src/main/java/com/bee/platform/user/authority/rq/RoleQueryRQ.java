package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName RoleQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 11:12$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("角色功能查询请求信息-不分页")
public class RoleQueryRQ implements Serializable {

    private static final long serialVersionUID = 4712202064565408927L;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("子系统类型")
    private String subType;
}

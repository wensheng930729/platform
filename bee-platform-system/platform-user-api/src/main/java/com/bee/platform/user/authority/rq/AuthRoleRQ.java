package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName AuthRoleRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/20$ 17:01$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("角色列表查询请求信息")
public class AuthRoleRQ implements Serializable {

    private static final long serialVersionUID = 3629911020012849626L;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("子系统类型")
    private String sysType;

    @ApiModelProperty("创建开始时间")
    private String createStartTime;

    @ApiModelProperty("创建截止时间")
    private String createEndTime;
}

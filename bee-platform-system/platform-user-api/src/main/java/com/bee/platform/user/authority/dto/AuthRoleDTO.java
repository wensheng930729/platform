package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName AuthRoleDTO
 * @Description 角色详细信息
 * @author jie.chen
 * @Date 2019/5/20$ 14:37$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("角色列表信息")
public class AuthRoleDTO implements Serializable {

    private static final long serialVersionUID = -6345474790320680856L;

    @ApiModelProperty("角色id")
    private Integer id;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("角色级别")
    private Integer level;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("是否开通")
    private boolean openStatu;

    @ApiModelProperty("关联账号数量")
    private Integer relatedNumber;
}

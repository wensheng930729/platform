package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName AppRoleDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/4/29$ 15:40$
 * @version 1.0.0
 */


@Data
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业产品角色信息")
public class AppRolesDTO implements Serializable {

    private static final long serialVersionUID = 2976337180958189375L;

    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("产品id")
    private Long appId;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("审核状态")
    private Integer auditStatus;

}

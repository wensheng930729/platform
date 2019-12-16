package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName AuthRoleDTO
 * @Description 企业下所有已开通功能
 * @author liliang TODO 待确定删不删
 * @Date 2019/5/24$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("企业下所有已开通功能")
public class AuthEnterpriseFunDTO implements Serializable {

    private static final long serialVersionUID = -6345474790320680856L;

    @ApiModelProperty("角色id")
    private Integer id;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("应用下开通的功能")
    private List<AuthEnterpriseFunDTO> children;
}

package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName AuthUserRoleRoleIdsDTO
 * @Description 功能描述
 * @Date 2019/5/24 17:21
 **/


@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("拥有个企业下拥有的角色ids")
public class AuthUserRoleRoleIdsDTO {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    @ApiModelProperty("角色ids")
    private List<Integer> roleIds;


}

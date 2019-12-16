package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Getter
@Setter
@ToString
@Accessors(chain = true)
public class AuthPlatformUserEnterpriseIdDto implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = -3983239030663993147L;

	@ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("归属公司id")
    private Integer enterpriseId;

    @ApiModelProperty("手机号")
    private String phone;

    @ApiModelProperty("角色类型用户类型:1application 2function 3custom 4base 5enterprise_admin  6super_admin 7 Other")
    private String roleType;
}

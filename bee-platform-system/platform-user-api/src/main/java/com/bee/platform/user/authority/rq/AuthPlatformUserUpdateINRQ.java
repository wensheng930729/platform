package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("中台用户状态信息传输对象")
public class AuthPlatformUserUpdateINRQ implements Serializable {

	 @ApiModelProperty("用户id")
	 private Integer id;
	 
	 @ApiModelProperty("公司id")
	 @NotNull
	 private Integer enterpriseId;
	 
	 @ApiModelProperty("是否启用：1启用 0禁用")
	 private Integer status;
}

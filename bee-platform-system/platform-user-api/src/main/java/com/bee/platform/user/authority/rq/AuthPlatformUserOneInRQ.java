package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("中台一个用户条件查询传输对象")
public class AuthPlatformUserOneInRQ implements Serializable{
	 @ApiModelProperty("公司id")
	 private Integer enterpriseId;

	 @ApiModelProperty("职务id")
	 private Integer postId;
	 
	 @ApiModelProperty("用户id")
	 private Integer id;
	 
	 @ApiModelProperty("部门Id")
	 private Integer departmentId;
}

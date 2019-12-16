package com.bee.platform.user.rq;

import java.io.Serializable;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("部门删除信息")
public class DepartmentsRQ implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = -7617551386162753591L;

	@ApiModelProperty("部门id")
	private Integer departmentId;
	
	@ApiModelProperty("是否删除")
	private Integer deleted;
}

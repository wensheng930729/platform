package com.bee.platform.user.authority.dto;

import java.io.Serializable;

import com.bee.platform.user.authority.rq.AuthPlatformUserDepartmentAndPostRQ;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("关联公司部门职位名称")
public class EdpNameDTO implements Serializable{
	
	@ApiModelProperty("归属公司简称")
    private String enterpriseSimpleName;
	
	@ApiModelProperty("部门名称")
    private String departmentName;
	
	@ApiModelProperty("职位名称")
    private String postName;

}

package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("首页企业信息封装类")
public class IndexEnterpriseDTO implements Serializable {
	
	private static final long serialVersionUID = -1443559995654177592L;

	@ApiModelProperty(value = "成员总数")
	private int totalMembership;

	@ApiModelProperty(value = "部门总数")
	private int totalDepartments;
}

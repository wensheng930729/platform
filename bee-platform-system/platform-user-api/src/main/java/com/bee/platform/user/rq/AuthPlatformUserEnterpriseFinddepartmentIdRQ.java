package com.bee.platform.user.rq;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("查询企业用户关联表的用户id传的条件")
public class AuthPlatformUserEnterpriseFinddepartmentIdRQ implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = -8253849672560354870L;
	/**
	 * 部门id
	 */
	private Integer departmentId;
	/**
	 * 企业id
	 */
	private Integer enterpriseId;
}

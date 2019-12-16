package com.bee.platform.user.dto;

import java.io.Serializable;
import java.sql.Date;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("列表信息")
public class EnterprisesCheckApplyDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("enterprisesCheckId")
	private Integer id;
	
	@ApiModelProperty("公司名称")
	private String name;
	
	@ApiModelProperty("公司地址")
	private String address;
	
	@ApiModelProperty("管理员")
	private String admin;
	
	@ApiModelProperty("提交时间")
	private Date createAt;
	
	@ApiModelProperty("企业的状态(0: 未通过，1: 已通过, 2: 未审核|信息变更, 3: 未审核|新提交)")
    private Integer type;
	
}

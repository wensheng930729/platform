package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
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
@ApiModel("查询销售订单下拉框返回参数")
public class ErpPucharseOrderBoxDTO implements Serializable{

	private static final long serialVersionUID = 1L;

	@ApiModelProperty("订单id")
	private Integer id;

	@ApiModelProperty("合同编号")
	private String contractNo;

	@ApiModelProperty("公司id")
	private Integer company;

	@ApiModelProperty("公司名称")
	private String companyName;

}

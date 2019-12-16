package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("编辑销售订单传参")
public class ErpSaleOrderUpdateRQ implements Serializable{


	private static final long serialVersionUID = 1L;
	@ApiModelProperty("销售单id")
	private Integer id;
	
	@ApiModelProperty("公司id")
	@NotNull(message = "公司id不能为空")
    private Integer company;

	@ApiModelProperty("公司名称")
	@NotEmpty(message = "公司名称不能为空")
    private String companyName;

	@ApiModelProperty("销售订单号")
	@NotEmpty(message = "销售订单号不能为空")
    private String contractNo;
    
	@ApiModelProperty("客户id")
	@NotNull(message = "客户id不能为空")
    private Integer customer;

	@ApiModelProperty("客户名称")
	@NotEmpty(message = "客户名称不能为空")
    private String customerName;

	@ApiModelProperty("签订日期")
	@NotNull(message = "签订日期不能为空")
	@JsonFormat(pattern = "yyyy-MM-dd")
	private Date contractDate;
	
	@ApiModelProperty("销售模式:0包运，1自提")
	@NotNull(message = "销售模式不能为空")
	private Integer sellMethod;
	
	@ApiModelProperty("合同质量要求")
    private String ContractQualityRequirements;
	
	@ApiModelProperty("备注")
	private String remark;

	@ApiModelProperty("销售员id")
	private Integer saleUserId;

}

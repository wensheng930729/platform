package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.bee.platform.customer.dto.ErpSaleOrderDetailDTO;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("查询销售订单返回参数")
public class ErpSaleOrderQueryDTO implements Serializable{

	private static final long serialVersionUID = 1L;

		@ApiModelProperty("id")
	 	private Integer id;

		@ApiModelProperty("订单号")
	    private String contractNo;

		@ApiModelProperty("订单id")
	    private Integer orderId;

		@ApiModelProperty("公司id")
	    private Integer company;

		@ApiModelProperty("公司名称")
	    private String companyName;

		@ApiModelProperty("签订日期")
		@JsonFormat(pattern = "yyyy-MM-dd")
	    private Date contractDate;

		@ApiModelProperty("客户名称")
	    private String customerName;

		@ApiModelProperty("客户id")
	    private Integer customer;

		@ApiModelProperty("产品id")
	    private Integer productId;

		@ApiModelProperty("产品名称")
	    private String productName;

		@ApiModelProperty("产品数量")
	    private BigDecimal num;

		@ApiModelProperty("单位")
	    private String unit;

		@ApiModelProperty("金额")
	    private BigDecimal amount;
	    
		@ApiModelProperty("执行状态")
	    private Integer state;

		@ApiModelProperty("合同单价")
	    private BigDecimal taxPrice;

		@ApiModelProperty("质量要求")
	    private String contractQualityRequirements;

		@ApiModelProperty("合同数量")
	    private Integer contractNum;

		@ApiModelProperty("销售模式:0包运，1自提")
	    private Integer sellMethod;

		@ApiModelProperty("销售结算单详情中结算情况")
	    private List<ErpSaleStatementOrderDetailDTO> saleStatementOrderDetailList;

		@ApiModelProperty("销售订单详细列表返回相关的DTO")
	    private List<ErpSaleOrderDetailDTO> saleOrderDetailList;

		@ApiModelProperty("销售员id")
		private Integer saleUserId;

		@ApiModelProperty("销售员姓名")
		private String saleUserName;

}

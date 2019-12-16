package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.bee.platform.datadriver.rq.ErpLogisticsStatusDetailRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsStatusRQ;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("返回物流结算的数据")
public class ErpLogisticsSettlementDTO implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = -4771397336996154050L;

	@ApiModelProperty("物流结算id")
	private Integer id;

	@ApiModelProperty("物流订单id")
	private Integer orderId;

	@ApiModelProperty("结算单号")
	private String statementNo;

	@ApiModelProperty("结算日期")
	@JsonFormat(pattern = "yyyy-MM-dd")
	private Date statementTime;

	@ApiModelProperty("合同重量")
	private BigDecimal contractWeight;

	@ApiModelProperty("收货重量")
	private BigDecimal receivedWeight;

	@ApiModelProperty("结算重量")
	private BigDecimal settlementWeight;

	@ApiModelProperty("合同单价")
	private BigDecimal contractPrice;

	@ApiModelProperty("结算单价")
	private BigDecimal settlementPrice;

	@ApiModelProperty("结算扣款")
	private BigDecimal settlementDeductions;

	@ApiModelProperty("结算总额")
	private BigDecimal settlementAmount;

	@ApiModelProperty("创建时间")
	private Date createTime;

	@ApiModelProperty("跟新时间")
	private Date updateTime;

	@ApiModelProperty(" 创建人id")
	private Integer createUser;

	@ApiModelProperty(" 修改人id")
	private Integer updateUser;
}

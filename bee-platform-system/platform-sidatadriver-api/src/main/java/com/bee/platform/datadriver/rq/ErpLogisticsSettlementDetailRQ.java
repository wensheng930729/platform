package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("物流结算明细请求数据")
public class ErpLogisticsSettlementDetailRQ implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@NotNull(message = "结算单号不能为空")
	@ApiModelProperty("结算单号")
    private String statementNo;
 
	@NotNull(message = "结算日期不能为空")
	@ApiModelProperty("结算日期")
	@JsonFormat(pattern = "yyyy-MM-dd")
    private Date statementTime;
   
	@NotNull(message = "合同重量不能为空")
	@ApiModelProperty("合同重量")
    private BigDecimal contractWeight;
   
	@NotNull(message = "收货重量不能为空")
	@ApiModelProperty("收货重量")
    private BigDecimal receivedWeight;
   
	@NotNull(message = "结算重量不能为空")
	@ApiModelProperty("结算重量")
    private BigDecimal settlementWeight;
   
	@NotNull(message = "合同单价不能为空")
	@ApiModelProperty("合同单价")
    private BigDecimal contractPrice;
   
	@NotNull(message = "结算单价不能为空")
	@ApiModelProperty("结算单价")
    private BigDecimal settlementPrice;
  
	@ApiModelProperty("结算扣款")
    private BigDecimal settlementDeductions;
   
	@NotNull(message = "结算总额不能为空")
	@ApiModelProperty("结算总额")
    private BigDecimal settlementAmount;
}

package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("添加物流付款需要传的数据")
public class ErpLogisticsPaymentRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 2047327136734535476L;

	
	@ApiModelProperty("物流付款id")
    private Integer id;
  
	@ApiModelProperty("付款单号")
    private String payOrderNo;
  
	@NotNull(message = "付款时间不能为空")
	@ApiModelProperty("付款时间")
    private String payTime;
   
	@NotNull(message = "物流订单id不能为空")
	@ApiModelProperty("物流订单id")
    private Integer orderId;
   
	@NotNull(message = "物流订单号不能为空")
	@ApiModelProperty("物流订单号")
    private String orderNo;
  
	@ApiModelProperty("公司id")
    private Integer companyId;
  
	@NotNull(message = "公司名称不能为空")
	@ApiModelProperty("公司名称")
    private String companyName;
   
	@ApiModelProperty("承运商id")
    private Integer carrierId;
   
	@NotNull(message = "承运商名称不能为空")
	@ApiModelProperty("承运商名称")
    private String carrierName;
    
	@NotNull(message = "付款金额不能为空")
	@ApiModelProperty("付款金额")
    private BigDecimal paymentAmount;
   
	@ApiModelProperty(" 支付类型,从码表取值")
    private String payType;
   
	@ApiModelProperty("备注")
    private String remarks;
	
}

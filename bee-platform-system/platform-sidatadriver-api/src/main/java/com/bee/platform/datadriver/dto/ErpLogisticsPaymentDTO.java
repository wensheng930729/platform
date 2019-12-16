package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.bee.platform.datadriver.rq.ErpLogisticsStatusDetailRQ;
import com.bee.platform.datadriver.rq.ErpLogisticsStatusRQ;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("物流订单付款状态需要传的数据")
public class ErpLogisticsPaymentDTO implements Serializable {
	/**
	* 
	*/
	private static final long serialVersionUID = 6286524864591555475L;
	@ApiModelProperty("物流付款状态id")
	private Integer id;

	@ApiModelProperty("付款单号")
	private String payOrderNo;

	@ApiModelProperty("付款时间")
	private String payTime;

	@ApiModelProperty("物流订单id")
	private Integer orderId;

	@ApiModelProperty("物流订单号")
	private String orderNo;

	@ApiModelProperty("付款金额")
	private BigDecimal paymentAmount;

	@ApiModelProperty("支付类型,从码表取值")
	private String payType;
	
	@ApiModelProperty("备注")
	private String remarks;

}

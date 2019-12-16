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
@ApiModel("物流结算要传的数据")
public class ErpLogisticsSettlementRQ implements Serializable{

	private static final long serialVersionUID = 1L;
	
	@NotNull(message = "物流订单id不能为空")
	@ApiModelProperty("物流订单id")
    private Integer orderId;

	@ApiModelProperty("物流结算明细请求数据")
    private List<ErpLogisticsSettlementDetailRQ> settlementDetails;

}

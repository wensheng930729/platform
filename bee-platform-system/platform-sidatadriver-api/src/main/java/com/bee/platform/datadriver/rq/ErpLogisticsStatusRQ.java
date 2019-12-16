package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("编辑物流订单状态需要传的数据")
public class ErpLogisticsStatusRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	//@NotEmpty(message = "物流状态明细id不能为空")
	@ApiModelProperty("物流订单id")
	private Integer id;
	
	@ApiModelProperty("产品id")
	private Integer productId;
	
	@ApiModelProperty("产品批次id")
	private Integer batchId;
	
	@NotNull(message = "物流订单状态：0是没有发货，1是在途中，2是已收货 不能为空")
    @ApiModelProperty("物流订单状态：0是没有发货，1是在途中，2是已收货")
    private Integer status;
    
	@NotNull(message = "预计到达天数不能为空")
    @ApiModelProperty("预计到达天数")
    private String estimatedArrivalTime;
    
	@NotNull(message = "发货时间不能为空")
    @ApiModelProperty("发货时间")
    private String deliveryTime;
    
	//@NotEmpty(message = "物流订单明细不能为空")
    @ApiModelProperty("物流订单明细")
    private List<ErpLogisticsStatusDetailRQ> list;
    
}

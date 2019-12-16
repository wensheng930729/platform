package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.List;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("编辑物流订单状态需要传的数据")
public class ErpLogisticsLogisticsTrackingRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = -8520314011640404850L;

	@ApiModelProperty("物流订单号")
    private String orderNumber;
	
	@ApiModelProperty("承运商名字")
	private String carrierName;
	
	@ApiModelProperty("物流订单状态：0是没有发货，1是在途中，2是已收货")
    private Integer status;
	
	@ApiModelProperty("当前用户所在企业及子企业id列表")
	private List<Integer> enterpriseIdList;

}

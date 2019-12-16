package com.bee.platform.datadriver.dto;

import java.io.Serializable;
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
@ApiModel("编辑物流订单状态返回的数据")
public class ErpLogisticsLogisticsTrackingDTO implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = -1544320901796978307L;

	@ApiModelProperty("物流订单id")
	private Integer id;
	
	@ApiModelProperty("产品批次名字")
	private String batchName;
	
	@ApiModelProperty("产品批次id")
	private Integer batchId;
	
	@ApiModelProperty("公司名称")
	private String name;
	
	@ApiModelProperty("物流订单号")
    private String orderNumber;
	
	@ApiModelProperty("签订日期")
    private Date signingTime;
	
	@ApiModelProperty("承运商id")
	private Integer carrierId;

	@ApiModelProperty("承运商名字")
	private String carrierName;
	
	@ApiModelProperty("承运商品名字")
	private String productName;
	
	@ApiModelProperty("'起始地")
	private String origin;
	
	@ApiModelProperty("到达地")
	private String destination;
	
	@ApiModelProperty("预计到达天数")
    private String estimatedArrivalTime;
	
	@ApiModelProperty("物流订单状态：0是没有发货，1是在途中，2是已收货")
    private Integer status;
}

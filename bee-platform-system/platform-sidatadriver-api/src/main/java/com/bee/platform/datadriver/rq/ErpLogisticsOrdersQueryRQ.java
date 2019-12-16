package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("物流订单列表请求的信息")
public class ErpLogisticsOrdersQueryRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("物流订单号")
    private String orderNumber;
	
	@ApiModelProperty("承运商名字")
	private String carrierName;
	
	@ApiModelProperty("承运商品名字")
	private String productName;
	
	@ApiModelProperty("开始时间")
	@JsonFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    private Date startTime;
	
	@ApiModelProperty("结束时间")
	@JsonFormat(pattern = "yyyy-MM-dd hh:mm:ss")
    private Date endTime;
	
	@ApiModelProperty("付款状态：1是已付款，2是未付款")
	private Integer payStatus;
	
	@ApiModelProperty("物流订单状态：0是没有发货，1是在途中，2是已收货")
    private Integer status;

	@ApiModelProperty("当前用户所在企业及子企业id列表")
	private List<Integer> enterpriseIdList;
}

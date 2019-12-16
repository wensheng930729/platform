package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

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
@ApiModel(value = "查询物流订单列表返回的DTO")
public class ErpLogisticsOrdersDTO implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("物流订单id")
	private Integer id;
	
	@ApiModelProperty("物流订单号")
    private String orderNumber;

	@ApiModelProperty("公司id")
    private Integer companyId;
	
	@ApiModelProperty("公司名字")
    private String name;
	
	@ApiModelProperty("签订日期")
    private Date signingTime;
	
	@ApiModelProperty("承运商id")
	private Integer carrierId;

	@ApiModelProperty("承运商名字")
	private String carrierName;
	
	@ApiModelProperty("起始地")
    private String origin;
   
	@ApiModelProperty("到达地")
    private String destination;
	
	@ApiModelProperty("承运商品名字")
	private String productName;
	
	@ApiModelProperty("承运商品数量")
    private BigDecimal number;
	
	@ApiModelProperty("预计到达天数")
    private String estimatedArrivalTime;
	
	@ApiModelProperty("付款状态：1是已付款，2是未付款")
	private Integer payStatus;

	@ApiModelProperty("含税金额")
	private BigDecimal taxAmount;
	
	@ApiModelProperty("物流订单状态：0是没有发货，1是在途中，2是已收货")
    private Integer status;

	@ApiModelProperty("产品id")
	private Integer productId;

	@ApiModelProperty("批次名称")
	private String batchName;

	@ApiModelProperty("批次id")
	private Integer batchId;

	@ApiModelProperty("单位")
	private String unit;
	
	
}

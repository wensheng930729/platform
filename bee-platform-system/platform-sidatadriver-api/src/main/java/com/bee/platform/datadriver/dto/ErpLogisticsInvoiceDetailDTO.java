package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("返回物流发票明细的数据")
public class ErpLogisticsInvoiceDetailDTO implements Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6014012130122947131L;

	@ApiModelProperty("物流发票明细id")
	private Integer id;

	@ApiModelProperty("物流订单id")
	private Integer orderId;

	@ApiModelProperty("物流发票id")
	private Integer invoiceId;

	@ApiModelProperty("产品id")
	private Integer productId;

	@ApiModelProperty("产品名称")
	private String productName;
	
	@ApiModelProperty("产品批次id")
	private Integer batchId;

	@ApiModelProperty("产品批次名称")
	private String batchName;

	@ApiModelProperty("产品单位")
	private String unit;

	@ApiModelProperty("重量")
	private BigDecimal weight;

	@ApiModelProperty("单价")
	private BigDecimal unitPrice;

	@ApiModelProperty("税率")
	private BigDecimal taxRate;

	@ApiModelProperty("总金额")
	private BigDecimal amount;

	@ApiModelProperty("创建时间")
	private Date createTime;

	@ApiModelProperty("修改时间")
	private Date updateTime;

	@ApiModelProperty("创建人id")
	private Integer createUser;

	@ApiModelProperty("修改人id")
	private Integer updateUser;

	@ApiModelProperty("删除：0是未删除，1是删除")
	private Integer deleted;
	
	
}

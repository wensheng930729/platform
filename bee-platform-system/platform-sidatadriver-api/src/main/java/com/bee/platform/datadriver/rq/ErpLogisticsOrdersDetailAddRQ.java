package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

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
@ApiModel("添加物流订单明细需要传的数据")
public class ErpLogisticsOrdersDetailAddRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@ApiModelProperty("物流订单明细id")
	private Integer id;

	@NotNull(message = "承运商品名字不能为空")
	@ApiModelProperty("承运商品名字")
	private String productName;
	
	@ApiModelProperty("商品id")
    private Integer productId;
	
	@ApiModelProperty("批次id")
    private Integer batchId;
    
	@ApiModelProperty("批次名称")
    private String batchName;
	
	@NotNull(message = "承运商品数量不能为空")
	@ApiModelProperty("承运商品数量")
    private BigDecimal number;
	
	@NotNull(message = "产品单位不能为空")
	@ApiModelProperty("产品单位")
    private String unit;
	
	@NotNull(message = "含税运费单价不能为空")
	@ApiModelProperty(" 含税运费单价")
    private BigDecimal unitPrice;
	
	@ApiModelProperty(" 税率")
    private BigDecimal taxRate;
 
	@NotNull(message = "无税金额不能为空")
	@ApiModelProperty("无税金额")
    private BigDecimal freeTaxAmount;
  
	@ApiModelProperty(" 税额")
    private BigDecimal tax;
	
	@NotNull(message = "含税金额不能为空")
	@ApiModelProperty("含税金额")
	private BigDecimal taxAmount;
	
}

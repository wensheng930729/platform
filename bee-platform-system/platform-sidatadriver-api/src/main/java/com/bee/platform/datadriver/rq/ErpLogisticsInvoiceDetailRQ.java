package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.math.BigDecimal;
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
@ApiModel("物流发票明细要传的数据")
public class ErpLogisticsInvoiceDetailRQ implements Serializable{

	 /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("物流发票明细id")
    private Integer id;
   
	@NotNull(message = "物流订单id不能为空")
	@ApiModelProperty("物流订单id")
    private Integer orderId;
   
	@NotNull(message = "物流发票id不能为空")
	@ApiModelProperty("物流发票id")
    private Integer invoiceId;
  
	@NotNull(message = "产品id不能为空")
	@ApiModelProperty("产品id")
	private Integer productId;

	@NotNull(message = "批次id不能为空")
	@ApiModelProperty("批次id")
	private Integer batchId;
	
	@NotNull(message = "产品批次名称不能为空")
	@ApiModelProperty("产品批次名称")
	private String batchName;
  
	@NotNull(message = " 产品名称不能为空")
	@ApiModelProperty(" 产品名称")
    private String productName;
   
	@ApiModelProperty("产品单位")
    private String unit;
   
	@NotNull(message = " 重量不能为空")
	@ApiModelProperty("重量")
    private BigDecimal weight;
  
	@NotNull(message = "单价不能为空")
	@ApiModelProperty("单价")
    private BigDecimal unitPrice;
  
	@ApiModelProperty("税率")
    private BigDecimal taxRate;
  
	@NotNull(message = "总金额不能为空")
	@ApiModelProperty("总金额")
    private BigDecimal amount;
	
	@ApiModelProperty("删除：0是未删除，1是删除")
    private Integer deleted;
}

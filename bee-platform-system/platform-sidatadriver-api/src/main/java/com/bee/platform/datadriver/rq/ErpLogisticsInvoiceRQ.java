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

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("物流发票要传的数据")
public class ErpLogisticsInvoiceRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("物流发票id")
    private Integer id;
  
    @NotNull(message = "物流订单id不能为空")
	@ApiModelProperty("物流订单id")
    private Integer orderId;

   	@ApiModelProperty("发票单号")
    private String invoiceNumber;
  
    @NotNull(message = "物流订单号不能为空")
   	@ApiModelProperty("物流订单号")
    private String orderNumber;
   
    @NotNull(message = "公司id不能为空")
   	@ApiModelProperty("公司id")
    private Integer companyId;
   
    @NotNull(message = "公司名称不能为空")
   	@ApiModelProperty("公司名称")
    private String companyName;
  
    @NotNull(message = "开发票时间不能为空")
   	@ApiModelProperty("开发票时间")
    private Date invoiceTime;
   
    @NotNull(message = "承运商id不能为空")
   	@ApiModelProperty("承运商id")
    private Integer carrierId;
    
    @NotNull(message = "承运商名称不能为空")
   	@ApiModelProperty("承运商名称")
    private String carrierName;
 
   	@ApiModelProperty("备注")
    private String remarks;
    
	@ApiModelProperty("物流发票详情集合")
    private List<ErpLogisticsInvoiceDetailRQ> list;

}

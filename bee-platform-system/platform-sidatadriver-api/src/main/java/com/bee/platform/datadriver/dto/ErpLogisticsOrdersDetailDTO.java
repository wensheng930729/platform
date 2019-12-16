package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
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
@ApiModel("返回物流订单明细以及批次")
public class ErpLogisticsOrdersDetailDTO implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 4068688864182522121L;

	
	@ApiModelProperty("物流订单明细id")
    private Integer id;
    
	@ApiModelProperty("物流订单id")
    private Integer orderId;
  
	@ApiModelProperty("商品id")
    private Integer productId;
   
	@ApiModelProperty("产品名称")
    private String productName;
   
	@ApiModelProperty("批次id")
    private Integer batchId;
    
	@ApiModelProperty("批次名称")
    private String batchName;
    
	@ApiModelProperty(" 产品单位")
    private String unit;
  
	@ApiModelProperty("数量")
    private BigDecimal number;
  
	@ApiModelProperty("含税运费单价")
    private BigDecimal unitPrice;
  
	@ApiModelProperty("税率")
    private BigDecimal taxRate;
  
	@ApiModelProperty("无税金额")
    private BigDecimal freeTaxAmount;
  
	@ApiModelProperty("税额")
    private BigDecimal tax;
   
	@ApiModelProperty("含税金额")
    private BigDecimal taxAmount;
   
	@ApiModelProperty("创建人id")
    private Integer createUser;
  
	@ApiModelProperty("修改人id")
    private Integer updateUser;
   
    private Date createTime;
   
    private Date updateTime;
   
    @ApiModelProperty("删除：0是未删除，1是删除")
    private Integer deleted;
}

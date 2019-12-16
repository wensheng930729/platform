package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("修改物流订单状态详情需要传的数据")
public class ErpLogisticsStatusDetailRQ implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@ApiModelProperty("物流订单详情id")
	private Integer id;
	
	@ApiModelProperty("产品id")
    private Integer productId;
   
    @ApiModelProperty("产品名称")
    private String productName;
   
    @ApiModelProperty("物流订单id")
    private Integer orderId;
   
    @ApiModelProperty("车牌号")
    private String plateNumber;
   
    @ApiModelProperty("合同重量")
    private BigDecimal contractWeight;
    
    @ApiModelProperty("载货重量")
    private BigDecimal loadingWeight;
    
    @ApiModelProperty("计量单位")
    private String unit;
   
    @ApiModelProperty("毛重")
    private BigDecimal roughWeight;
   
    @ApiModelProperty("皮重")
    private BigDecimal tare;
   
    @ApiModelProperty("净重量")
    private BigDecimal suttle;
 
    @ApiModelProperty("创建时间")
    private Date createTime;
   
    @ApiModelProperty("更新时间")
    private Date updateTime;
    
    @ApiModelProperty("创建人id")
    private Integer createUser;
   
    @ApiModelProperty("修改人id")
    private Integer updateUser;
    
    @ApiModelProperty("逻辑删除")
    private Integer deleted;
}

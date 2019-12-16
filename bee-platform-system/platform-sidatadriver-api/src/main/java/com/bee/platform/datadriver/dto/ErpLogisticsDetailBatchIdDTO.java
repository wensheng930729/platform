package com.bee.platform.datadriver.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("返回订单明细批次集合")
public class ErpLogisticsDetailBatchIdDTO implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = -5043534433225786213L;

	@ApiModelProperty("批次id")
    private Integer batchId;
    
	@ApiModelProperty("批次名称")
    private String batchName;
	
	@ApiModelProperty("数量")
    private BigDecimal number;
	
	
}

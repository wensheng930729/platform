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
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("返回物流订单明细以及批次集合")
public class ErpLogisticsOrdersDetailBatchIdDTO implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 7878046361550601941L;

	@ApiModelProperty("商品id")
    private Integer productId;
	
	@ApiModelProperty("商品id")
    private String productName;
	
	@ApiModelProperty(" 产品单位")
    private String unit;
	
	private List<ErpLogisticsDetailBatchIdDTO> batchList;
}

package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.NotEmpty;

import com.bee.platform.datadriver.dto.ErpLogisticsLogisticsTrackingDTO;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("修改物流发货明细传的数据")
public class ErpLogisticsShippingDetailsRQ implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = -1884739620852039962L;
	
	@ApiModelProperty("物流订单详情id")
	private Integer id;
	
    @ApiModelProperty("物流订单明细")
    private List<ErpLogisticsStatusDetailRQ> list;

}

package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.util.List;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("添加物流付款明细需要传的数据")
public class ErpLogisticsPaymentRemarksRQ implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = -3199903951306590926L;

	@ApiModelProperty("物流订单id")
    private Integer id;
	
	@ApiModelProperty("公司id")
    private Integer companyId;
  
	@NotNull(message = "公司名称不能为空")
	@ApiModelProperty("公司名称")
    private String companyName;
   
	@ApiModelProperty("承运商id")
    private Integer carrierId;
   
	@NotNull(message = "承运商名称不能为空")
	@ApiModelProperty("承运商名称")
    private String carrierName;
	
	@ApiModelProperty("备注")
    private String remarks;
	
	@ApiModelProperty("物流付款明细集合")
	private List<ErpLogisticsPaymentRQ> list;
}

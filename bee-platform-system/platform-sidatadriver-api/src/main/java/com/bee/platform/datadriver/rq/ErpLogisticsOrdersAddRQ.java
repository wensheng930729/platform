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
@ApiModel("添加物流订单需要传的数据")
public class ErpLogisticsOrdersAddRQ implements Serializable{/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("物流订单id")
	private Integer id;
	
	@ApiModelProperty("公司id")
    private Integer companyId;
	
	@ApiModelProperty("公司名称")
    private String companyName;
	
	@ApiModelProperty("物流订单号")
    private String orderNumber;
	
	@NotNull(message = "签订日期不能为空")
	@ApiModelProperty("签订日期")
    private Date signingTime;
	
	@NotNull(message = "物流类型不能为空")
	@ApiModelProperty("物流类型")
    private Integer type;
	
	@NotNull(message = "订单来源（相当与销售采购订单）不能为空")
	@ApiModelProperty("订单来源（相当与销售采购订单）")
    private String sourceOrder;

	@ApiModelProperty("订单来源（相当与销售采购订单）id")
	private Integer sourceOrderId;
	
	@ApiModelProperty("承运商id")
	private Integer carrierId;

	@NotNull(message = "承运商名字不能为空")
	@ApiModelProperty("承运商名字")
	private String carrierName;
	
	@NotNull(message = "起始地不能为空")
	@ApiModelProperty("起始地")
    private String origin;
	
	@NotNull(message = "到达地不能为空")
	@ApiModelProperty("到达地")
    private String destination;
	
	@ApiModelProperty("备注")
    private String remarks;
	
	@ApiModelProperty("发货时间")
    private String deliveryTime;

	@ApiModelProperty("物流订单状态：0是没有发货，1是在途中，2是已收货")
	private Integer status;
	
	@NotNull(message = "物流明细不能为空")
	@ApiModelProperty("物流明细list")
	private List<ErpLogisticsOrdersDetailAddRQ> list;
}

package com.bee.platform.business.dto;

import java.io.Serializable;
import java.util.Date;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * 
 * @author chenxm66777123
 *
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "首页现货信息")
public class SaleProductDTO implements Serializable {
	
	private static final long serialVersionUID = -3829356142485957297L;
	
	@ApiModelProperty("id")
	private Long id;
	
	@ApiModelProperty("订单编号")
	private String sn;
	
	@ApiModelProperty(value = "商品名称")
	private String name;

	@ApiModelProperty(value = "属性/规格/指标")
    private String taste;

	@ApiModelProperty(value = "可供数量")
	private String residualTotal;
	
	@ApiModelProperty(value = "发布时间")
	private Date releaseTime;
	
	@ApiModelProperty("地区")
	private String address;
	
	@ApiModelProperty("付款方式")
    private String paymentMethod;
	
	@ApiModelProperty("单价")
	private Double price;
	
	@ApiModelProperty("发起公司名称")
	private String initiateCompanyName;
	

	
}

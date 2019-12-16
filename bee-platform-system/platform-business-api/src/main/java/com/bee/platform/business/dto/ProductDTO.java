package com.bee.platform.business.dto;

import java.io.Serializable;

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
@ApiModel(value = "首页询价单信息")
public class ProductDTO implements Serializable {
	
	private static final long serialVersionUID = -3829356142485957297L;
	
	@ApiModelProperty("id")
	private Long id;

	@ApiModelProperty("询价单标题")
	private String title;
	
	@ApiModelProperty("采购量")
	private String buyCount;
	
	@ApiModelProperty("地区")
	private String address;
	
	@ApiModelProperty("报价企业数量")
	private Integer buyCompCount;
	
	@ApiModelProperty("发布时间")
	private String releaseTime;
	
	@ApiModelProperty("公司名称")
	private String company;
	
	@ApiModelProperty("时间百分比")
	private String timePercentage;
	
	@ApiModelProperty("剩余日期")
	private Integer RemainDay;
}

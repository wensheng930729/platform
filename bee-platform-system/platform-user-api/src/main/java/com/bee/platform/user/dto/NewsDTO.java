package com.bee.platform.user.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * 条件查询发布的资讯
 * @author xuzheng
 *
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel(value = "查询资讯请求实体")
public class NewsDTO implements Serializable{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	//标题
	@ApiModelProperty("标题")
	private String title;
	//类型
	@ApiModelProperty("类型")
	private Integer type;
	//开始时间
	@ApiModelProperty("开始时间")
	private String startTime;
	//结束时间
	@ApiModelProperty("结束时间")
	private String endTime;
}

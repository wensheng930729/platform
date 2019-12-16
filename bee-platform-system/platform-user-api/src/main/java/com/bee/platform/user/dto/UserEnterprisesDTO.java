package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 用户企业对象
 * @author: junyang.li
 * @create: 2019-03-18 11:01
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("企业信息相关数据")
public class UserEnterprisesDTO implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 8820354295836689684L;

	@ApiModelProperty("企业id")
    private Integer id;

    @ApiModelProperty("企业名称")
    private String name;

    @ApiModelProperty("企业logo")
    private String head;

    @ApiModelProperty("企业联系电话")
    private String contact;

    @ApiModelProperty("企业地址")
    private String address;

    @ApiModelProperty("企业类型 1企业 2物流商")
    private Integer type;

    @ApiModelProperty("企业类型 1企业 2物流商")
    private String typeDesc;

    @ApiModelProperty("企业所属行业")
    private String industry;
}

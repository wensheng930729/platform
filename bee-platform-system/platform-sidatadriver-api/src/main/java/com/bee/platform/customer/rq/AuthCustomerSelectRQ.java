package com.bee.platform.customer.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "客户列表查询相关")
public class AuthCustomerSelectRQ implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1953334838776740131L;

	@ApiModelProperty("客户id")
    private Integer cusName;

    @ApiModelProperty("客户一级分类")
    private String cusFirstType;

    @ApiModelProperty("客户二级分类")
    private Integer cusSecondType;

    @ApiModelProperty("用户状态：1启用 0禁用")
    private Integer status;
}

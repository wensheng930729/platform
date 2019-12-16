package com.bee.platform.customer.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "客户下拉列表返回公司下的客户列表相关的DTO")
public class AuthCustomerBoxDto {

	@ApiModelProperty("客户表主键ID")
    private Integer id;

    @ApiModelProperty("客户姓名")
    private String cusName;

}

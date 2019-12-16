package com.bee.platform.customer.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.Set;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "客户列表返回相关的DTO")
public class AuthCustomerListDto {

	@ApiModelProperty("客户表主键ID")
    private Integer id;

    @ApiModelProperty("企业ID")
    private Integer enterpriseId;

    @ApiModelProperty("客户编码")
    private String cusNo;

    @ApiModelProperty("客户姓名")
    private String cusName;

    @ApiModelProperty("客户一级分类")
    private Set<String> cusFirstType;

    @ApiModelProperty("客户二级分类")
    private Set<String> cusSecondType;

    @ApiModelProperty("用户状态：1启用 0禁用")
    private Integer status;

}

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
public class CustomerFirstCategoryBoxDto {

	@ApiModelProperty("客户一级分类code")
    private String value;

    @ApiModelProperty("客户一级分类name")
    private String name;

}

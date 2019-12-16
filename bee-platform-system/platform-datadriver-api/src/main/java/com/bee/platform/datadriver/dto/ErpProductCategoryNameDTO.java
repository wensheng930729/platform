package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpProductCategoryNameDTO
 * @Description 功能描述
 * @Date 2019/6/4 10:26
 **/

@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("产品名称类别名称返回信息")
public class ErpProductCategoryNameDTO {

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品类别名称")
    private String categoryName;
}

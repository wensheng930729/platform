package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpProductListByCategoryDTO
 * @Description 功能描述
 * @Date 2019/7/16 9:55
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("根据分类查询产品列表返回信息")
public class ErpProductListByCategoryDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品分类名称")
    private String categoryName;


}

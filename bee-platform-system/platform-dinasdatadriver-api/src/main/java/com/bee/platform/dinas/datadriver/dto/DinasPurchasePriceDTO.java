package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName DinasProductList2DTO
 * @Description 功能描述
 * @Date 2019/8/14 10:07
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购税前价格返回信息")
public class DinasPurchasePriceDTO implements Serializable {

    private static final long serialVersionUID = 339831722189299481L;

    @ApiModelProperty("不含税单价")
    private BigDecimal priceBefore;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPriceBefore;
}

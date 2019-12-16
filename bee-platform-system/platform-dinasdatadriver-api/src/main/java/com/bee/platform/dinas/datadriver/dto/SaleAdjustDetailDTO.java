package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description 销售合同调价函明细返回信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同调价函明细返回信息")
public class SaleAdjustDetailDTO implements Serializable {
    private static final long serialVersionUID = -2857173612951954279L;

    @ApiModelProperty("调价函明细id")
    private Integer id;

    @ApiModelProperty("调价函id")
    private Integer adjustId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String specName;

    @ApiModelProperty("计量单位")
    private String unit;

    @ApiModelProperty("不含税单价")
    private BigDecimal priceBefore;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPriceBefore;

    @ApiModelProperty("调价后不含税单价")
    private BigDecimal priceAfter;

    @ApiModelProperty("调价后含税单价")
    private BigDecimal taxPriceAfter;

}

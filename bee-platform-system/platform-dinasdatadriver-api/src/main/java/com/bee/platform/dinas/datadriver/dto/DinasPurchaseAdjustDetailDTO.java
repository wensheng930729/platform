package com.bee.platform.dinas.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 采购调价明细表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购调价明细返回信息")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class DinasPurchaseAdjustDetailDTO implements Serializable {

    private static final long serialVersionUID = -167724346332311585L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("调价主表id")
    private Integer adjustId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("不含税单价")
    private BigDecimal priceBefore;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPriceBefore;

    @ApiModelProperty("调价后单价")
    private BigDecimal priceAfter;

    @ApiModelProperty("调价后含税单价")
    private BigDecimal taxPriceAfter;

}

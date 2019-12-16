package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
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
@ApiModel("采购调价明细请求信息")
public class DinasPurchaseAdjustDetailRQ implements Serializable {

    private static final long serialVersionUID = 3364365124186372185L;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产品规格")
    @NotNull(message = "产品id不能为空")
    private Integer productSpecId;

    @ApiModelProperty("不含税单价")
    @NotNull(message = "不含税单价不能为空")
    private BigDecimal priceBefore;

    @ApiModelProperty("含税单价")
    @NotNull(message = "含税单价不能为空")
    private BigDecimal taxPriceBefore;

    @ApiModelProperty("调价后单价")
    @NotNull(message = "调价后单价不能为空")
    private BigDecimal priceAfter;

    @ApiModelProperty("调价后含税单价")
    @NotNull(message = "调价后含税单价不能为空")
    private BigDecimal taxPriceAfter;

}

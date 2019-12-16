package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 成本模拟基础配置
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel(value = "erpc成本模拟计算结果的rq")
public class ErpCostSimulationCalculationRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("现货单价")
    private BigDecimal spotUnitPrice;

    @ApiModelProperty("期货单价")
    private BigDecimal futuresUnitPrice;

    @ApiModelProperty("运费")
    private BigDecimal freight;

    @ApiModelProperty("港口服务费")
    private BigDecimal portServiceFee;

    @ApiModelProperty("成本模拟成本配置")
    private ErpCostAllocationSimulationRQ costAllocation;

    @ApiModelProperty("成本模拟指数录入")
    private ErpCostIndexInputRQ costIndexInput;

}

package com.bee.platform.costcontroller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 成本模拟计算结果
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel(value = "erpc成本模拟计算结果的DTO")
public class ErpCostSimulationComputedResultDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("成品品味")
    private BigDecimal endProductTaste;
    /**
     * 碳耗
     */
    @ApiModelProperty("碳耗")
    private BigDecimal carbonConsumption;
    /**
     * 矿耗
     */
    @ApiModelProperty("矿耗")
    private BigDecimal oreConsumption;
    /**
     * 50矿耗
     */
    @ApiModelProperty("50矿耗")
    private BigDecimal fiftyOreConsumption;
    /**
     * 矿成本
     */
    @ApiModelProperty("矿成本")
    private BigDecimal miningCost;
    /**
     * 碳成本
     */
    @ApiModelProperty("碳成本")
    private BigDecimal carbonCost;
    /**
     * 电成本
     */
    @ApiModelProperty("电成本")
    private BigDecimal electricityCost;
    /**
     * 辅料成本
     */
    @ApiModelProperty("辅料成本")
    private BigDecimal accessoriesCost;
    /**
     * 三费
     */
    @ApiModelProperty("三费")
    private BigDecimal threeCharges;
    /**
     * 制造成本
     */
    @ApiModelProperty("制造成本")
    private BigDecimal manufacturingCost;
    /**
     * 50完全成本（列表需要的取值）
     */
    @ApiModelProperty("50完全成本")
    private BigDecimal fiftyFullCost;
    /**
     * 完全成本
     */
    @ApiModelProperty("完全成本")
    private BigDecimal fullCost;

}

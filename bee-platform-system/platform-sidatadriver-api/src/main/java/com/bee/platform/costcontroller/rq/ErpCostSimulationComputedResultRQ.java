package com.bee.platform.costcontroller.rq;

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
 * 成本模拟计算结果
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel(value = "erpc成本模拟计算结果添加的rq")
public class ErpCostSimulationComputedResultRQ implements Serializable {

    private static final long serialVersionUID = 1L;


//    @ApiModelProperty("id")
//    private Integer id;
    /**
     * 成品品味
     */
    @ApiModelProperty("成品品味")
    @NotNull(message = "成品品味不能为空")
    private BigDecimal endProductTaste;
    /**
     * 碳耗
     */
    @ApiModelProperty("碳耗")
    @NotNull(message = "碳耗不能为空")
    private BigDecimal carbonConsumption;
    /**
     * 矿耗
     */
    @ApiModelProperty("矿耗")
    @NotNull(message = "矿耗不能为空")
    private BigDecimal oreConsumption;
    /**
     * 50矿耗
     */
    @ApiModelProperty("50矿耗")
    @NotNull(message = "50矿耗不能为空")
    private BigDecimal fiftyOreConsumption;
    /**
     * 矿成本
     */
    @ApiModelProperty("矿成本")
    @NotNull(message = "矿成本不能为空")
    private BigDecimal miningCost;
    /**
     * 碳成本
     */
    @ApiModelProperty("碳成本")
    @NotNull(message = "碳成本不能为空")
    private BigDecimal carbonCost;
    /**
     * 电成本
     */
    @ApiModelProperty("电成本")
    @NotNull(message = "电成本不能为空")
    private BigDecimal electricityCost;
    /**
     * 辅料成本
     */
    @ApiModelProperty("辅料成本")
    @NotNull(message = "辅料成本不能为空")
    private BigDecimal accessoriesCost;
    /**
     * 三费
     */
    @ApiModelProperty("三费")
    @NotNull(message = "三费不能为空")
    private BigDecimal threeCharges;
    /**
     * 制造成本
     */
    @ApiModelProperty("制造成本")
    @NotNull(message = "制造成本不能为空")
    private BigDecimal manufacturingCost;
    /**
     * 50完全成本（列表需要的取值）
     */
    @ApiModelProperty("50完全成本")
    @NotNull(message = "50完全成本不能为空")
    private BigDecimal fiftyFullCost;
    /**
     * 完全成本
     */
    @ApiModelProperty("完全成本")
    @NotNull(message = "完全成本不能为空")
    private BigDecimal fullCost;
    /**
     * 类型(1报盘 2快检 3自检)
     */
    @ApiModelProperty("类型(1报盘 2快检 3自检)")
    @NotNull(message = "类型不能为空")
    private Integer type;


}

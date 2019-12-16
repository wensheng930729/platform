package com.bee.platform.costcontroller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 料批模拟计算结果返回信息
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批模拟计算结果返回信息")
public class ErpCostMaterialSimulationResultDTO implements Serializable {
    private static final long serialVersionUID = -481722486051105338L;

    @ApiModelProperty("计算结果id")
    private Integer id;

    @ApiModelProperty("料批详情")
    private String materialDetail;

    @ApiModelProperty("理论成品品位")
    private BigDecimal grade;

    @ApiModelProperty("料批矿耗")
    private BigDecimal materialMineCost;

    @ApiModelProperty("50矿耗")
    private BigDecimal fiftyMineCost;

    @ApiModelProperty("碳耗")
    private BigDecimal carbonCost;

    @ApiModelProperty("50完全成本")
    private BigDecimal fiftyFullCost;

    @ApiModelProperty("完全成本")
    private BigDecimal fullCost;

    @ApiModelProperty("状态 1确认 2未确认")
    private Integer status;
}

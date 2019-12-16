package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 料批模拟原料成分请求信息
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("料批模拟计算结果请求信息")
public class ErpCostMaterialSimulationResultRQ implements Serializable {

    private static final long serialVersionUID = 4438007118525717308L;

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

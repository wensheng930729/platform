package com.bee.platform.costcontroller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 料批模拟原料成分返回信息
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("料批模拟原料成分返回信息")
public class ErpCostMaterialSimulationElementDTO implements Serializable {

    private static final long serialVersionUID = -634677097327372505L;

    @ApiModelProperty("原料id")
    private Integer id;

    @ApiModelProperty("原料名称")
    private String materialName;

    @ApiModelProperty("Cr2O3")
    private BigDecimal chromiumTrioxide;

    @ApiModelProperty("FeO")
    private BigDecimal ironOxide;

    @ApiModelProperty("MgO")
    private BigDecimal magnesia;

    @ApiModelProperty("Al2O3")
    private BigDecimal aluminumOxide;

    @ApiModelProperty("铬铁比")
    private BigDecimal chromiumIronRatio;

    @ApiModelProperty("镁铝比")
    private BigDecimal magnesiumAluminumRatio;

    @ApiModelProperty("现货到厂价")
    private BigDecimal price;

    @ApiModelProperty("料批配比")
    private BigDecimal materialRatio;
}

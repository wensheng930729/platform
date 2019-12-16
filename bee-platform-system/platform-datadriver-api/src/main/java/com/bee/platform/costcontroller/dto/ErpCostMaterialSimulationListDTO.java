package com.bee.platform.costcontroller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批模拟列表返回信息")
public class ErpCostMaterialSimulationListDTO implements Serializable {
    private static final long serialVersionUID = -7367577126309157440L;

    @ApiModelProperty("料批模拟id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("料批名称")
    private String name;

    @ApiModelProperty("成本配置id")
    private Integer allocationId;

    @ApiModelProperty("成本配置名称")
    private String allocationName;

    @ApiModelProperty("50矿耗")
    private BigDecimal fiftyMineCost;

    @ApiModelProperty("50完全成本")
    private BigDecimal fiftyFullCost;

    @ApiModelProperty("理论成品品位")
    private BigDecimal grade;

}

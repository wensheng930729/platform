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
@ApiModel(value = "erpc成本模拟更新的rq")
public class ErpCostSimulationUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("成本模拟id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

//    @ApiModelProperty("成本模拟名称")
//    private String name;

    @ApiModelProperty("供应商id")
    private Integer supplierId;

    @ApiModelProperty("供应商")
    private String supplier;

    @ApiModelProperty("询盘人id")
    private Integer inquirerId;

    @ApiModelProperty("询盘人")
    private String inquirer;

    @ApiModelProperty("成本配置id")
    private Integer costAllocationId;

    @ApiModelProperty("原料")
    private String rawMaterial;

    @ApiModelProperty("交易港口")
    private String tradePort;

    @ApiModelProperty("现货单价")
    private BigDecimal spotUnitPrice;

    @ApiModelProperty("期货单价")
    private BigDecimal futuresUnitPrice;

    @ApiModelProperty("运费")
    private BigDecimal freight;

    @ApiModelProperty("港口服务费")
    private BigDecimal portServiceFee;

    @ApiModelProperty("成本模拟成本配置(JSON)")
    private ErpCostAllocationSimulationRQ costAllocation;

    @ApiModelProperty("成本模拟指数录入")
    private List<ErpCostIndexInputRQ> costIndexInputList;

    @ApiModelProperty("成本模拟计算结果")
    private List<ErpCostSimulationComputedResultRQ> costSimulationComputedResultList;


}

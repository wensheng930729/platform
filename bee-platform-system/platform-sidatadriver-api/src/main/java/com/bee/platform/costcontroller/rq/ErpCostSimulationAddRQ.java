package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
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
@ApiModel(value = "erpc成本模拟添加的rq")
public class ErpCostSimulationAddRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

//    @ApiModelProperty("成本模拟名称")
//    @NotEmpty(message = "成本模拟名称不能为空")
//    private String name;

    @ApiModelProperty("供应商id")
    private Integer supplierId;

    @ApiModelProperty("供应商")
    @NotEmpty(message = "供应商不能为空")
    private String supplier;

    @ApiModelProperty("询盘人")
    @NotEmpty(message = "询盘人不能为空")
    private String inquirer;

    @ApiModelProperty("成本配置id")
    @NotNull(message = "成本配置不能为空")
    private Integer costAllocationId;

    @ApiModelProperty("原料")
    @NotEmpty(message = "原料不能为空")
    private String rawMaterial;

    @ApiModelProperty("交易港口")
    @NotEmpty(message = "交易港口不能为空")
    private String tradePort;

    @ApiModelProperty("现货单价")
    @NotNull(message = "不能为空")
    private BigDecimal spotUnitPrice;

    @ApiModelProperty("期货单价")
    private BigDecimal futuresUnitPrice;

    @ApiModelProperty("运费")
    @NotNull(message = "运费不能为空")
    private BigDecimal freight;

    @ApiModelProperty("港口服务费")
    private BigDecimal portServiceFee;

    @ApiModelProperty("成本模拟成本配置(JSON)")
    @NotNull(message = "成本模拟成本配置不能为空")
    private ErpCostAllocationSimulationRQ costAllocation;

    @ApiModelProperty("成本模拟指数录入")
    @NotNull(message = "成本模拟指数录入不能为空")
    private List<ErpCostIndexInputRQ> costIndexInputList;

    @ApiModelProperty("成本模拟计算结果")
    @NotNull(message = "成本模拟计算结果不能为空")
    private List<ErpCostSimulationComputedResultRQ> costSimulationComputedResultList;


}

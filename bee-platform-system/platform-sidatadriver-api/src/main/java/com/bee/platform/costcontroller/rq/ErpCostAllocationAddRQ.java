package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Max;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * erp成本小工具-成本配置
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel(value = "erpc成本配置添加的rq")
public class ErpCostAllocationAddRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

    @ApiModelProperty("成本配置名称")
    @NotEmpty(message = "成本配置名称不能为空")
    private String name;

    @ApiModelProperty("铬回收率")
    @NotNull(message = "铬回收率不能为空")
    @Max(value= 100,message = "铬回收率不能大于100%")
    private BigDecimal chromiumRecoveryRate;

    @ApiModelProperty("铁回收率")
    @NotNull(message = "铁回收率不能为空")
    @Max(value = 100,message = "铁回收率不能大于100%")
    private BigDecimal ironRecoveryRate;

    @ApiModelProperty("硅含量")
    @NotNull(message = "硅含量不能为空")
    private BigDecimal siliconContent;

    @ApiModelProperty("碳含量")
    @NotNull(message = "碳含量不能为空")
    private BigDecimal carbonContent;

    @ApiModelProperty("运输损耗")
    @NotNull(message = "运输损耗不能为空")
    @Max(value = 100,message = "运输损耗不能大于100%")
    private BigDecimal transportLoss;

    @ApiModelProperty("外币汇率")
    @NotNull(message = "外币汇率不能为空")
    private BigDecimal foreignExchangeRate;

    @ApiModelProperty("炉电单价")
    @NotNull(message = "炉电单价不能为空")
    private BigDecimal furnaceElectricityUnitPrice;

    @ApiModelProperty("炉电吨耗")
    @NotNull(message = "炉电吨耗不能为空")
    private BigDecimal furnaceElectricityTonConsume;

    @ApiModelProperty("动力电单价")
    @NotNull(message = "动力电单价不能为空")
    private BigDecimal electricityUnitPrice;

    @ApiModelProperty("动力电耗")
    @NotNull(message = "动力电耗不能为空")
    private BigDecimal electricityConsume;

    @ApiModelProperty("固定碳")
    @NotNull(message = "固定碳不能为空")
    private BigDecimal fixedCarbon;

    @ApiModelProperty("利用率")
    @NotNull(message = "利用率不能为空")
    private BigDecimal utilizationRate;

    @ApiModelProperty("焦炭单价")
    @NotNull(message = "焦炭单价不能为空")
    private BigDecimal cokeUnitPrice;

    @ApiModelProperty("焦炭比例")
    @NotNull(message = "焦炭比例不能为空")
    private BigDecimal cokeRatio;

    @ApiModelProperty("兰炭单价")
    @NotNull(message = "兰炭单价不能为空")
    private BigDecimal semiCokeUnitPrice;

    @ApiModelProperty("兰炭比例")
    @NotNull(message = "兰炭比例不能为空")
    private BigDecimal semiCokeRatio;

    @ApiModelProperty("电极糊单价")
    @NotNull(message = "电极糊单价不能为空")
    private BigDecimal electrodePasteUnitPrice;

    @ApiModelProperty("电极糊吨耗")
    @NotNull(message = "电极糊吨耗不能为空")
    private BigDecimal electrodePasteTonConsume;

    @ApiModelProperty("硅石单价")
    @NotNull(message = "硅石单价不能为空")
    private BigDecimal silicaUnitPrice;

    @ApiModelProperty("硅石吨耗")
    @NotNull(message = "硅石吨耗不能为空")
    private BigDecimal silicaTonConsume;

    @ApiModelProperty("制造费用")
    @NotNull(message = "制造费用不能为空")
    private BigDecimal manufacturingCost;

    @ApiModelProperty("辅料易耗")
    @NotNull(message = "辅料易耗不能为空")
    private BigDecimal consumableAccessorie;

    @ApiModelProperty("直接人工")
    @NotNull(message = "直接人工不能为空")
    private BigDecimal directLabor;

    @ApiModelProperty("劳务")
    @NotNull(message = "劳务不能为空")
    private BigDecimal labourService;

    @ApiModelProperty("管理费用")
    @NotNull(message = "管理费用不能为空")
    private BigDecimal managementCost;

    @ApiModelProperty("销售费用")
    @NotNull(message = "销售费用不能为空")
    private BigDecimal saleCost;

    @ApiModelProperty("财务费用")
    @NotNull(message = "财务费用不能为空")
    private BigDecimal financeCost;

}

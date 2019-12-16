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
 * erp成本小工具-成本配置
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel(value = "erpc成本配置详情DTO")
public class ErpCostAllocationDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @ApiModelProperty("主键id")
    private Integer id;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 成本配置名称
     */
    @ApiModelProperty("成本配置名称")
    private String name;
    /**
     * 铬回收率
     */
    @ApiModelProperty("铬回收率")
    private BigDecimal chromiumRecoveryRate;
    /**
     * 铁回收率
     */
    @ApiModelProperty("铁回收率")
    private BigDecimal ironRecoveryRate;
    /**
     * 硅含量
     */
    @ApiModelProperty("硅含量")
    private BigDecimal siliconContent;
    /**
     * 碳含量
     */
    @ApiModelProperty("碳含量")
    private BigDecimal carbonContent;
    /**
     * 运输损耗
     */
    @ApiModelProperty("运输损耗")
    private BigDecimal transportLoss;
    /**
     * 外币汇率
     */
    @ApiModelProperty("外币汇率")
    private BigDecimal foreignExchangeRate;
    /**
     * 炉电单价
     */
    @ApiModelProperty("炉电单价")
    private BigDecimal furnaceElectricityUnitPrice;
    /**
     * 炉电吨耗
     */
    @ApiModelProperty("炉电吨耗")
    private BigDecimal furnaceElectricityTonConsume;
    /**
     * 动力电单价
     */
    @ApiModelProperty("动力电单价")
    private BigDecimal electricityUnitPrice;
    /**
     * 动力电耗
     */
    @ApiModelProperty("动力电耗")
    private BigDecimal electricityConsume;
    /**
     * 固定碳
     */
    @ApiModelProperty("固定碳")
    private BigDecimal fixedCarbon;
    /**
     * 利用率
     */
    @ApiModelProperty("利用率")
    private BigDecimal utilizationRate;
    /**
     * 焦炭单价
     */
    @ApiModelProperty("焦炭单价")
    private BigDecimal cokeUnitPrice;
    /**
     * 焦炭比例
     */
    @ApiModelProperty("焦炭比例")
    private BigDecimal cokeRatio;
    /**
     * 兰炭单价
     */
    @ApiModelProperty("兰炭单价")
    private BigDecimal semiCokeUnitPrice;
    /**
     * 兰炭比例
     */
    @ApiModelProperty("兰炭比例")
    private BigDecimal semiCokeRatio;
    /**
     * 电极糊单价
     */
    @ApiModelProperty("电极糊单价")
    private BigDecimal electrodePasteUnitPrice;
    /**
     * 电极糊吨耗
     */
    @ApiModelProperty("电极糊吨耗")
    private BigDecimal electrodePasteTonConsume;
    /**
     * 硅石单价
     */
    @ApiModelProperty("硅石单价")
    private BigDecimal silicaUnitPrice;
    /**
     * 硅石吨耗
     */
    @ApiModelProperty("硅石吨耗")
    private BigDecimal silicaTonConsume;
    /**
     * 制造费用
     */
    @ApiModelProperty("制造费用")
    private BigDecimal manufacturingCost;
    /**
     * 辅料易耗
     */
    @ApiModelProperty("辅料易耗")
    private BigDecimal consumableAccessorie;
    /**
     * 直接人工
     */
    @ApiModelProperty("直接人工")
    private BigDecimal directLabor;
    /**
     * 劳务
     */
    @ApiModelProperty("劳务")
    private BigDecimal labourService;
    /**
     * 管理费用
     */
    @ApiModelProperty("管理费用")
    private BigDecimal managementCost;
    /**
     * 销售费用
     */
    @ApiModelProperty("销售费用")
    private BigDecimal saleCost;
    /**
     * 财务费用
     */
    @ApiModelProperty("财务费用")
    private BigDecimal financeCost;

    /**
     * 状态：1启动 0禁用
     */
    @ApiModelProperty("状态：1启动 0禁用")
    private Integer status;
}

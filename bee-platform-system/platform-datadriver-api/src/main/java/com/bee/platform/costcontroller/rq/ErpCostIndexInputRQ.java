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
 * 成本指标录入
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel(value = "erpc成本模拟指标录入添加的rq")
public class ErpCostIndexInputRQ implements Serializable{

    private static final long serialVersionUID = 1L;

//    @ApiModelProperty("id")
//    private Integer id;
    /**
     * 三氧化二铬
     */
    @ApiModelProperty("三氧化二铬")
    @NotNull(message = "三氧化二铬不能为空")
    private BigDecimal chromiumTrioxide;
    /**
     * 氧化亚铁
     */
    @ApiModelProperty("氧化亚铁")
    @NotNull(message = "氧化亚铁不能为空")
    private BigDecimal ferrousOxide;
    /**
     * 氧化镁
     */
    @ApiModelProperty("氧化镁")
    @NotNull(message = "氧化镁不能为空")
    private BigDecimal magnesiumOxide;
    /**
     * 三氧化二铝
     */
    @ApiModelProperty("三氧化二铝")
    @NotNull(message = "三氧化二铝不能为空")
    private BigDecimal aluminumTrioxide;
    /**
     * 铬/铁比例
     */
    @ApiModelProperty("铬/铁比例")
    @NotNull(message = "铬/铁比例不能为空")
    private BigDecimal ferrochromeRatio;
    /**
     * 镁/铝比例
     */
    @ApiModelProperty("镁/铝比例")
    @NotNull(message = "镁/铝比例不能为空")
    private BigDecimal magnesiumAluminiumRatio;
    /**
     * 类型(1报盘 2快检 3自检)
     */
    @ApiModelProperty("类型(1报盘 2快检 3自检)")
    @NotNull(message = "类型(1报盘 2快检 3自检)不能为空")
    private Integer type;



}

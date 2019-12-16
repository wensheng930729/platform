package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel(value = "erpc成本模拟查询的rq")
public class ErpCostSimulationQueryRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("供应商id")
    private Integer supplierId;

    @ApiModelProperty("供应商")
    private String supplier;

    @ApiModelProperty("成本配置id")
    private Integer costAllocationId;

    @ApiModelProperty("原料")
    private String rawMaterial;

    @ApiModelProperty("状态：1启动 0禁用")
    private Integer status;
}

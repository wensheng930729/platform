package com.bee.platform.costcontroller.dto;

import com.bee.platform.costcontroller.rq.ErpCostAllocationSimulationRQ;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

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
@ApiModel(value = "erp成本模拟列表DTO")
public class ErpCostSimulationQueryDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @ApiModelProperty("主键")
    private Integer id;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 供应商id
     */
    @ApiModelProperty("供应商id")
    private Integer supplierId;
    /**
     * 供应商
     */
    @ApiModelProperty("供应商")
    private String supplier;
    /**
     * 成本配置id
     */
    @ApiModelProperty("成本配置id")
    private Integer costAllocationId;
    /**
     * 成本配置(JSON)
     */
    @ApiModelProperty("成本配置名称")
    private String costAllocationName;
    /**
     * 原料
     */
    @ApiModelProperty("原料")
    private String rawMaterial;
    /**
     * 交易港口
     */
    @ApiModelProperty("交易港口")
    private String tradePort;
    /**
     * 自检成本
     */
    @ApiModelProperty("自检成本")
    private BigDecimal selfCheckCost;
    /**
     * 快检成本
     */
    @ApiModelProperty("快检成本")
    private BigDecimal fastCheckCost;
    /**
     * 商检成本
     */
    @ApiModelProperty("商检成本")
    private BigDecimal commodityCheckCost;


}

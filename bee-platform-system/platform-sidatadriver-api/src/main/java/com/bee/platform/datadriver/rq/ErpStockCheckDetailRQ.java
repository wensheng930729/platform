package com.bee.platform.datadriver.rq;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 库存盘点明细
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存盘点明细请求参数")
public class ErpStockCheckDetailRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 盘点主单id
     */
    @ApiModelProperty("盘点主单id")
    @NotNull(message ="盘点主单id不能为空")
    private Integer stockCheckId;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产成品批次id")
    private Integer productBatchId;

    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id不能为空")
    private Integer repositoryId;

    /**
     * 计量单位
     */
    @ApiModelProperty("计量单位")
    @NotNull(message = "计量单位不能为空")
    private String unit;

//    /**
//     * 化验单id
//     */
//    @ApiModelProperty("化验单id")
//    @NotNull(message = "化验单id不能为空")
//    private Integer testReportId;

    /**
     * 理论数量
     */
    @ApiModelProperty("理论数量")
    @NotNull(message = "理论数量不能为空")
    private BigDecimal expectNumber;
    /**
     * 实际数量
     */
    @ApiModelProperty("实际数量")
    @NotNull(message = "实际数量不能为空")
    @Min(value = 0,message = "实际数量不能小于0")
    private BigDecimal realNumber;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

}

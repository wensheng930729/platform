package com.bee.platform.datadriver.rq;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 领料出库明细表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("领料出库明细请求参数")
public class ErpOutOfStockOrderDetailRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 领料出库主表id
     */
    @ApiModelProperty("领料出库主表id")
    @NotNull(message = "领料出库主表id不能为空")
    private Integer outOfStockOrderId;

    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;


    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产成品批次id")
//    @NotNull(message = "产品批次id不能为空")
    private Integer productBatchId;

    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id不能为空")
    private Integer repositoryId;

    /**
     * 仓库
     */
    @ApiModelProperty("仓库")
    private String storehouse;
    /**
     * 计量单位
     */
    @ApiModelProperty("计量单位")
    @NotEmpty(message = "计量单位不能为空")
    private String unit;
    /**
     * 化验单id
     */
    @ApiModelProperty("化验单id")
    @NotNull(message = "化验单id不能为空")
    private Integer testReportId;
    /**
     * 化验单编号
     */
    @ApiModelProperty("化验单编号")
    private String testReportCode;
    /**
     * 出库数量
     */
    @ApiModelProperty("出库数量")
    @NotNull(message = "出库数量不能为空")
    @Min(value = 0,message = "出库数量不能小于0")
    private BigDecimal number;


}

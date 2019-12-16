package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("料出库明细返回信息")
@JsonInclude
public class ErpOutOfStockOrderDetailDTO implements Serializable {

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
    private Integer outOfStockOrderId;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    private Integer productId;
    /**
     * 产品批次id
     */
    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;

    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
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
    private String unit;
    /**
     * 化验单id
     */
    @ApiModelProperty("化验单id")
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
    private BigDecimal number;

    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;


}

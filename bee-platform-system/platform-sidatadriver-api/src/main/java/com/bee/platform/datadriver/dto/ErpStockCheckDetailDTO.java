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
 * 库存盘点明细
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存盘点明细返回信息")
@JsonInclude
public class ErpStockCheckDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 盘点主单id
     */
    @ApiModelProperty("盘点主单id")
    private Integer stockCheckId;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;
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
    private String testCode;
    /**
     * 理论数量
     */
    @ApiModelProperty("理论数量")
    private BigDecimal expectNumber;
    /**
     * 实际数量
     */
    @ApiModelProperty("实际数量")
    private BigDecimal realNumber;

    @ApiModelProperty("公司id")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;


}

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
 * 库存表
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-03
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("现存明细查看详情返回参数")
@JsonInclude
public class ErpStockDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("企业id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;

    @ApiModelProperty("产品类别")
    private String productCategoryName;

    @ApiModelProperty("产品类别id")
    private Integer productCategoryId;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("仓库id")
    private Integer repositoryId;

    @ApiModelProperty("仓库名称")
    private String repositoryName;

    @ApiModelProperty("期初数量")
    private BigDecimal initNum;

    @ApiModelProperty("入库数量")
    private BigDecimal inStockNum;

    @ApiModelProperty("出库数量")
    private BigDecimal outStockNum;

    @ApiModelProperty("盘点差量正负值")
    private BigDecimal dNum;

    @ApiModelProperty("现存数量")
    private BigDecimal stockNum;




}

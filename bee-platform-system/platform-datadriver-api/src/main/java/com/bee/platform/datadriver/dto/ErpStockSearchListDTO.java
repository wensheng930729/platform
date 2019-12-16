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
@ApiModel("现存明细返回参数")
@JsonInclude
public class ErpStockSearchListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 企业id
     */
    @ApiModelProperty("企业id")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
    private String companyName;

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
    /**
     * 产品类别
     */
    @ApiModelProperty("产品类别")
    private String productCategory;
    /**
     * 单位
     */
    @ApiModelProperty("单位")
    private String unit;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String repositoryName;
    /**
     * 期初数量
     */
    @ApiModelProperty("期初数量")
    private BigDecimal initNum;
    /**
     * 入库数量
     */
    @ApiModelProperty("入库数量")
    private BigDecimal inStockNum;
    /**
     * 出库数量
     */
    @ApiModelProperty("出库数量")
    private BigDecimal outStockNum;
    /**
     * 盘点差量正负值
     */
    @ApiModelProperty("盘点差量正负值")
    private BigDecimal dNum;
    /**
     * 现存数量
     */
    @ApiModelProperty("现存数量")
    private BigDecimal stockNum;




}

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
 * @author dell
 * @version 1.0.0
 * @ClassName ErpStockCheckProductListDTO
 * @Description 功能描述
 * @Date 2019/6/4 20:26
 **/


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存盘点产品列表返回信息")
@JsonInclude
public class ErpStockCheckProductListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
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
     * 单位
     */
    @ApiModelProperty("单位")
    private String unit;


    /**
     * 理论数量
     */
    @ApiModelProperty("理论数量")
    private BigDecimal expectNumber;

}

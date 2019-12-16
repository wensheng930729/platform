package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同明细详情返回信息")
public class SaleOrderDetailDTO implements Serializable {
    private static final long serialVersionUID = 1903059652527532244L;

    @ApiModelProperty("明细id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String specName;

    @ApiModelProperty("计量单位")
    private String unit;

    @ApiModelProperty("不含税单价")
    private BigDecimal price;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPrice;

    @ApiModelProperty("数量")
    private BigDecimal num;

}

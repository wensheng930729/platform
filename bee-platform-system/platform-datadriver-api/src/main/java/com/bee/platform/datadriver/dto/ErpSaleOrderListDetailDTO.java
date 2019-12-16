package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @Classname ErpSaleOrderListDetailDTO
 * @Description 销售订单列表详情明细返回信息
 * @Date 2019/6/10 19:02
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("销售订单列表详情明细返回信息")
public class ErpSaleOrderListDetailDTO implements Serializable {
    private static final long serialVersionUID = 8261959901905324774L;

    @ApiModelProperty("订单明细id")
    private Integer id;

    @ApiModelProperty("订单id")
    private Integer orderId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPrice;

    @ApiModelProperty("税率")
    private String taxRate;

    @ApiModelProperty("税额")
    private BigDecimal taxAmount;

    @ApiModelProperty("含税金额")
    private BigDecimal amount;
}

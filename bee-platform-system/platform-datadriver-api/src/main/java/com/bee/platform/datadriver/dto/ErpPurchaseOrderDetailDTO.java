package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName ErpPurchaseOrderDetailDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/30$ 9:05$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单明细信息")
public class ErpPurchaseOrderDetailDTO implements Serializable {

    private static final long serialVersionUID = -3536675437367526251L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("采购单id")
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

    @ApiModelProperty("税率，从码表取值")
    private String taxRate;

    @ApiModelProperty("无税金额")
    private BigDecimal taxFreeAmount;

    @ApiModelProperty("税额")
    private BigDecimal taxAmount;

    @ApiModelProperty("含税金额")
    private BigDecimal amount;
}

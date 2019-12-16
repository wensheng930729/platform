package com.bee.platform.datadriver.rq;

import java.io.Serializable;
import java.math.BigDecimal;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;

@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("添加销售订单详情传参")
public class ErpSaleOrderDetailAddRQ implements Serializable{
    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
	private Integer id;
    /**
     * 关联的销售单id
     */
    @ApiModelProperty("关联的销售单id")
    @NotNull(message = "销售订单id不能为空")
    private Integer orderId;
    /**
     * 产品
     */
    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;
    /**
     * 产品数量
     */
    @ApiModelProperty("产品数量")
    @NotNull(message = "产品数量不能为空")
    private BigDecimal num;
    /**
     * 单位
     */
    @ApiModelProperty("单位")
    private String unit;
    /**
     * 含税单价
     */
    @ApiModelProperty("含税单价")
    @NotNull(message = "含税单价不能为空")
    private BigDecimal taxPrice;
    /**
     * 税率，从码表取值
     */
    @ApiModelProperty("税率")
    @NotNull(message = "税率不能为空")
    private String taxRate;
    /**
     * 税额
     */
    @ApiModelProperty("税额")
    @NotNull(message = "税额不能为空")
    private BigDecimal taxAmount;
    /**
     * 含税金额
     */
    @ApiModelProperty("含税金额")
    @NotNull(message = "含税金额不能为空")
    private BigDecimal amount;
    
}

package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author xin.huang
 * @description 销售合同明细请求信息
 * @date 2019/8/13
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同明细请求信息")
public class SaleOrderDetailRQ implements Serializable {
    private static final long serialVersionUID = 999580571273284145L;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("不含税单价")
    private BigDecimal price;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPrice;

    @ApiModelProperty("合同数量")
    private BigDecimal num;

}

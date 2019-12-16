package com.bee.platform.dinas.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName PurchaseOrderSaveRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 17:41$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单明细返回信息")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class DinasPurchaseOrderDetailDTO implements Serializable {

    private static final long serialVersionUID = 7834712846455094087L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("不含税单价")
    private BigDecimal price;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPrice;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("数量")
    private String num;

}

package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName ErpPurchaseInvoiceOrderDetailDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/1$ 15:10$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("发票明细保存请求信息")
public class ErpInvoiceOrderDetailRQ implements Serializable {

    private static final long serialVersionUID = -5660963750016754056L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("发票id")
    private Integer orderId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品批次id")
    private Integer batchId;

    /*@ApiModelProperty("产品批次名称")
    private String batchName;*/

    @ApiModelProperty("单价")
    private BigDecimal price;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("税率，从码表取值")
    private String taxRate;

    @ApiModelProperty("金额")
    private BigDecimal amount;
}

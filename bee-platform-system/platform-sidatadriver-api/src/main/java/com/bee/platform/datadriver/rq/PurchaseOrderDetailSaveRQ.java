package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName ErpPurchaseOrderDetailRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/29$ 11:04$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单明细保存请求信息")
public class PurchaseOrderDetailSaveRQ implements Serializable {

    private static final long serialVersionUID = 5205414066673148326L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("采购单id")
    private Integer orderId;

    @ApiModelProperty("产品")
    private Integer productId;

    @ApiModelProperty("产品批次id")
    private Integer batchId;

    /*@ApiModelProperty("产品批次名称")
    private String batchName;*/

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

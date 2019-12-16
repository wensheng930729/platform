package com.bee.platform.dinas.datadriver.rq;

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
@ApiModel("采购订单明细保存请求信息")
public class DinasPurchaseOrderDetailRQ implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("合同id")
    // @NotNull(message = "合同id不能为空")
    private Integer orderId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产品规格")
    @NotNull(message = "产品规格不能为空")
    private Integer productSpecId;

    @ApiModelProperty("不含税单价")
    @NotNull(message = "不含税单价不能为空")
    private BigDecimal price;

    @ApiModelProperty("含税单价")
    @NotNull(message = "含税单价不能为空")
    private BigDecimal taxPrice;

    @ApiModelProperty("数量")
    @NotNull(message = "数量不能为空")
    private BigDecimal num;
}

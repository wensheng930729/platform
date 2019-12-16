package com.bee.platform.datadriver.rq;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 销售收款单收款详情表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("销售收款明细请求参数")
public class ErpReceiptOrderDetailRQ implements Serializable {

    private static final long serialVersionUID = 1L;

//    @ApiModelProperty("id")
//    private Integer id;

    /**
     * 销售收款主表id
     */
    @ApiModelProperty("销售收款主表id")
    @NotNull(message = "销售收款主表id不能为空")
    private Integer receiptOrderId;

    @ApiModelProperty("销售订单id")
    @NotNull(message = "销售订单id不能为空")
    private Integer saleOrderId;

    /**
     * 销售订单编号
     */
    @ApiModelProperty("销售订单编号")
    @NotEmpty(message = "销售订单编号不能为空")
    private String saleCode;
    /**
     * 销售订单金额
     */
    @ApiModelProperty("销售订单金额")
    @NotNull(message = "销售订单金额不能为空")
    private BigDecimal saleOrderAmount;
    /**
     * 收款金额
     */
    @ApiModelProperty("本次收款金额")
    @NotNull(message = "本次收款金额不能为空")
    @Min(value = 0,message = "本次收款金额不能小于0")
    private BigDecimal receiptAmount;




}

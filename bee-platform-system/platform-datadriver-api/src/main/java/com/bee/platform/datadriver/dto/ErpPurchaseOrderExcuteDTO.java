package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName ErpPurchaseOrderDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/6$ 17:13$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单执行信息")
public class ErpPurchaseOrderExcuteDTO implements Serializable {

    private static final long serialVersionUID = -5824635233050085208L;

    @ApiModelProperty("收货品位")
    private BigDecimal receivedGrade;

    @ApiModelProperty("结算品位")
    private BigDecimal accountGrade;

    @ApiModelProperty("收货重量")
    private BigDecimal receivedWeight;

    @ApiModelProperty("结算重量")
    private BigDecimal accountWeight;

    @ApiModelProperty("品位盈亏")
    private BigDecimal gradeBalance;

    @ApiModelProperty("干吨盈亏")
    private BigDecimal dryBalance;

    @ApiModelProperty("已付金额")
    private BigDecimal payAmount;

    @ApiModelProperty("执行状态（0已签订 1执行中 2发货完成/收货完成 3已结算 4已收款/已付款")
    private Integer excuteStatu;

}

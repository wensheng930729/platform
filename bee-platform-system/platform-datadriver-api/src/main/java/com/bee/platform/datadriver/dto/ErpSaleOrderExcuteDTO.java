package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @Classname ErpSaleOrderExcuteDTO
 * @Description 销售订单执行信息
 * @Date 2019/6/10 19:49
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售订单执行信息")
public class ErpSaleOrderExcuteDTO implements Serializable {
    private static final long serialVersionUID = -6246088008899590736L;

    @ApiModelProperty("发货品位")
    private BigDecimal receivedGrade;

    @ApiModelProperty("结算品位")
    private BigDecimal accountGrade;

    @ApiModelProperty("发货数量")
    private BigDecimal receivedWeight;

    @ApiModelProperty("结算数量")
    private BigDecimal accountWeight;

    @ApiModelProperty("品位盈亏")
    private BigDecimal gradeBalance;

    @ApiModelProperty("干重盈亏")
    private BigDecimal dryBalance;

    @ApiModelProperty("已收金额")
    private BigDecimal payAmount;
}

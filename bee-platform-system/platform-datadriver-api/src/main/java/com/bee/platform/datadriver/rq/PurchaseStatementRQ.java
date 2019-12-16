package com.bee.platform.datadriver.rq;

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
 * @Classname PurchaseStatementRQ
 * @Description 采购结算单请求信息
 * @Date 2019/5/30 16:07
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购结算单请求信息")
public class PurchaseStatementRQ implements Serializable {
    private static final long serialVersionUID = -8406761892610600726L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("结算单号")
    @NotEmpty(message = "结算单号不能为空")
    private String code;

    @ApiModelProperty("采购单id")
    @NotNull(message = "采购订单id不能为空")
    private Integer orderId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

    @ApiModelProperty("进厂干重")
    private BigDecimal srcDryWeight;

    @ApiModelProperty("结算干重")
    private BigDecimal realDryWeight;

    @ApiModelProperty("干重盈亏")
    private BigDecimal dryBalance;

    @ApiModelProperty("结算金额")
    private BigDecimal realAmount;

    @ApiModelProperty("结算湿重")
    private BigDecimal realWetWeight;

    @ApiModelProperty("结算水分")
    private BigDecimal realWater;

    @ApiModelProperty("结算扣款")
    private BigDecimal realDebit;

    @ApiModelProperty("结算单价")
    private BigDecimal realPrice;

    @ApiModelProperty("进厂湿吨")
    private BigDecimal srcWetWeight;

    @ApiModelProperty("扣水")
    private BigDecimal waterDebit;

    @ApiModelProperty("实际进厂成本")
    private BigDecimal cost;

    @ApiModelProperty("发票数量")
    private Integer invoiceCount;

    @ApiModelProperty("发票金额")
    private BigDecimal invoiceAmount;

    @ApiModelProperty("结算日期")
    private String statementTime;

    @ApiModelProperty("附件地址")
    private String url;

    @ApiModelProperty("结算品位")
    private BigDecimal grade;
}

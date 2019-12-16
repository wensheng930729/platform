package com.bee.platform.datadriver.dto;

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
 * @Classname ErpPurchaseStatementDetailDTO
 * @Description 采购结算单返回详细信息
 * @Date 2019/5/30 19:47
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购结算单返回详细信息")
public class ErpPurchaseStatementDetailDTO implements Serializable {
    private static final long serialVersionUID = 7363319468332810012L;

    @ApiModelProperty("结算单号")
    private String code;

    @ApiModelProperty("结算单id")
    private Integer id;

    @ApiModelProperty("采购单id")
    private Integer orderId;

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
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date statementTime;

    @ApiModelProperty("状态")
    private Integer state;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("采购公司名称")
    private String companyName;

    @ApiModelProperty("采购订单号")
    private String contractNo;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("附件地址")
    private String url;

    @ApiModelProperty("结算品位")
    private BigDecimal grade;

}

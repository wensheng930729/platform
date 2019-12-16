package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 采购结算单结算详情
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("采购结算单结算详情返回信息")
@JsonInclude
public class ErpPurchaseStmtDetailDTO {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("结算单id")
    private Integer statementId;

    @ApiModelProperty("结算单编号")
    private String statementCode;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("结算湿重")
    private BigDecimal realWetWeight;

    @ApiModelProperty("结算水分")
    private BigDecimal realWater;

    @ApiModelProperty("结算干重")
    private BigDecimal realDryWeight;

    @ApiModelProperty("结算扣款")
    private BigDecimal realDebit;

    @ApiModelProperty("结算单价")
    private BigDecimal realPrice;

    @ApiModelProperty("结算金额")
    private BigDecimal realAmount;

    @ApiModelProperty("进厂湿吨")
    private BigDecimal srcWetWeight;

    @ApiModelProperty("扣水")
    private BigDecimal waterDebit;

    @ApiModelProperty("进厂干重")
    private BigDecimal srcDryWeight;

    @ApiModelProperty("干吨盈亏")
    private BigDecimal dryBalance;

    @ApiModelProperty("实际进厂成本")
    private BigDecimal cost;

    @ApiModelProperty("发票数量")
    private Integer invoiceCount;

    @ApiModelProperty("发票金额")
    private BigDecimal invoiceAmount;

    @ApiModelProperty("结算日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date statementTime;

    @ApiModelProperty("grade")
    private BigDecimal grade;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产品批次名称")
    private String productBatchName;
}

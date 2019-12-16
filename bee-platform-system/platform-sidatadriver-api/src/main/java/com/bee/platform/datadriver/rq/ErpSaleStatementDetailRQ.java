package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Classname ErpSaleStatementDetailRQ
 * @Description 销售结算明细请求信息
 * @Date 2019/5/31 15:07
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售结算明细请求信息")
public class ErpSaleStatementDetailRQ implements Serializable {
    private static final long serialVersionUID = -7248471949215114138L;

    @ApiModelProperty("产品id")
    @NotEmpty(message = "产品id不能为空")
    private Integer product;

    @ApiModelProperty("产品名称")
    @NotEmpty(message = "产品名称不能为空")
    private String productName;

    @ApiModelProperty("车牌号")
    private String plateNo;

    @ApiModelProperty("结算日期")
    @NotNull(message = "结算日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("发货数量")
    private BigDecimal srcNum;

    @ApiModelProperty("收货数量")
    private BigDecimal receiveNum;

    @ApiModelProperty("结算数量")
    private BigDecimal realNum;

    @ApiModelProperty("发货品位")
    private BigDecimal srcGrade;

    @ApiModelProperty("收货品位")
    private BigDecimal receiveGrade;

    @ApiModelProperty("结算品位")
    private BigDecimal realGrade;

    @ApiModelProperty("品位误差")
    private BigDecimal gradeError;

    @ApiModelProperty("发货金额")
    private BigDecimal srcAmount;

    @ApiModelProperty("结算单价")
    private BigDecimal realPrice;

    @ApiModelProperty("结算金额")
    private BigDecimal realAmount;

    @ApiModelProperty("盈亏金额")
    private BigDecimal balance;

    @ApiModelProperty("结算扣款")
    private BigDecimal realDebit;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;
}

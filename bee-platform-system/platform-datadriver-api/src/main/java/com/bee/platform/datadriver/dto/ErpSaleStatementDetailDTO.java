package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Classname ErpSaleStatementDetailDTO
 * @Description 销售结算单明细详情返回信息
 * @Date 2019/5/31 17:21
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "销售结算单明细详情返回信息")
public class ErpSaleStatementDetailDTO implements Serializable {
    private static final long serialVersionUID = 3911614734920948315L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("结算单id")
    private Integer statementId;

    @ApiModelProperty("结算订单id")
    private Integer orderId;

    @ApiModelProperty("产品id")
    private Integer product;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("车牌号")
    private String plateNo;

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

    @ApiModelProperty("结算日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("状态")
    private Integer state;

    @ApiModelProperty("销售订单号")
    private String contractNo;

    @ApiModelProperty("结算单号")
    private String code;
}

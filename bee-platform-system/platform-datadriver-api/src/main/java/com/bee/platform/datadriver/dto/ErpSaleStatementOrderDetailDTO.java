package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @Classname ErpSaleStatementOrderDetailDTO
 * @Description 销售结算单详情中结算情况
 * @Date 2019/6/3 10:40
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("销售结算单详情中结算情况")
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ErpSaleStatementOrderDetailDTO implements Serializable {
    private static final long serialVersionUID = 7793443501445091544L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("结算单号")
    private String code;

    @ApiModelProperty("结算日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String recordTime;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品id")
    private Integer product;

    @ApiModelProperty("车号")
    private String plateNo;

    @ApiModelProperty("发货数量")
    private BigDecimal srcNum;

    @ApiModelProperty("发货品味")
    private BigDecimal srcGrade;

    @ApiModelProperty("发货金额")
    private BigDecimal srcAmount;

    @ApiModelProperty("收货数量")
    private BigDecimal receiveNum;

    @ApiModelProperty("结算数量")
    private BigDecimal realNum;

    @ApiModelProperty("收货品位")
    private BigDecimal receiveGrade;

    @ApiModelProperty("结算品位")
    private BigDecimal realGrade;

    @ApiModelProperty("品位误差")
    private BigDecimal gradeError;

    @ApiModelProperty("结算单价")
    private BigDecimal realPrice;

    @ApiModelProperty("结算金额")
    private BigDecimal realAmount;

    @ApiModelProperty("盈亏金额")
    private BigDecimal balance;
}

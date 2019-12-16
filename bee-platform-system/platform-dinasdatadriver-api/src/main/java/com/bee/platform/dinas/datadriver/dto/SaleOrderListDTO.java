package com.bee.platform.dinas.datadriver.dto;

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
 * @author xin.huang
 * @description 销售合同列表返回信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同列表返回信息")
public class SaleOrderListDTO implements Serializable {
    private static final long serialVersionUID = 540230180008483631L;

    @ApiModelProperty("合同id")
    private Integer id;

    @ApiModelProperty("合同编号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("订货商名称")
    private String customerName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("签订日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;

    @ApiModelProperty("已付款金额")
    private BigDecimal payment;

    @ApiModelProperty("可用金额")
    private BigDecimal availableAmount;
}

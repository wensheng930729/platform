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
 * @description 销售回款列表返回信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售回款列表返回信息")
public class SalePaymentListDTO implements Serializable {
    private static final long serialVersionUID = 2653392683952243035L;

    @ApiModelProperty("回款id")
    private Integer id;

    @ApiModelProperty("合同编号")
    private String orderCode;

    @ApiModelProperty("付款单号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("订货商名称")
    private String customerName;

    @ApiModelProperty("回款日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiveDate;

    @ApiModelProperty("回款金额")
    private BigDecimal receiveAmount;

}

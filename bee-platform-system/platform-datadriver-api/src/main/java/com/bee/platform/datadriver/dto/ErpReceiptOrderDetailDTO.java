package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 销售收款单收款详情表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("销售收款明细返回信息")
@JsonInclude
public class ErpReceiptOrderDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 销售收款主表id
     */
    @ApiModelProperty("销售收款主表id")
    private Integer receiptOrderId;


    @ApiModelProperty("销售订单id")
    private Integer saleOrderId;
    /**
     * 销售订单编号
     */
    @ApiModelProperty("销售订单编号")
    private String saleCode;
    /**
     * 销售订单金额
     */
    @ApiModelProperty("销售订单金额")
    private BigDecimal saleOrderAmount;
    /**
     * 收款金额
     */
    @ApiModelProperty("收款金额")
    private BigDecimal receiptAmount;
    /**
     * 累计收款金额
     */
    @ApiModelProperty("累计收款金额")
    private BigDecimal totalReceiptAmount;

    @ApiModelProperty("收款单号")
    private String receiptCode;

    @ApiModelProperty("收款日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptTime;

    @ApiModelProperty("支付方式")
    private String payMethod;





}

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
import java.util.List;

/**
 * @author xin.huang
 * @description 销售发票详情返回信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售发票详情返回信息")
public class SaleInvoiceDTO implements Serializable {
    private static final long serialVersionUID = 1572473886037500242L;

    @ApiModelProperty("发票id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("合同编号")
    private String orderCode;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("订货商名称")
    private String customerName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String specName;

    @ApiModelProperty("开票数量")
    private BigDecimal invoiceNum;

    @ApiModelProperty("开票金额")
    private BigDecimal invoiceAmount;

    @ApiModelProperty("发票附件")
    private String url;

    @ApiModelProperty("发票附件列表")
    private List<DinasUrlDTO> urlList;

    @ApiModelProperty("开票日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date invoiceDate;

}

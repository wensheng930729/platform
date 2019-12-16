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
 * @ClassName ErpPurchaseInvoiceOrderInfoDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/1$ 13:14$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购发票列表信息")
public class ErpPurchaseInvoiceOrderInfoDTO implements Serializable {

    private static final long serialVersionUID = 1902071085819378768L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("明细表Id")
    private Integer subId;

    @ApiModelProperty("采购单id")
    private Integer purchaseOrder;

    @ApiModelProperty("采购单编号")
    private String purchaseOrderNo;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("产品id")
    private String productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品批次id")
    private Integer batchId;

    @ApiModelProperty("产品批次名称")
    private String batchName;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("含税金额")
    private BigDecimal amount;

    @ApiModelProperty("开票日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date invoiceDate;

    @ApiModelProperty("状态")
    private Integer state;
}

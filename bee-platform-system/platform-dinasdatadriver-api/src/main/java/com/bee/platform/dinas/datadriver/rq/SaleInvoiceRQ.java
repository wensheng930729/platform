package com.bee.platform.dinas.datadriver.rq;

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
 * @description 销售发票请求信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售发票请求信息")
public class SaleInvoiceRQ implements Serializable {
    private static final long serialVersionUID = 790908409554599913L;

    @ApiModelProperty("发票id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("开票数量")
    private BigDecimal invoiceNum;

    @ApiModelProperty("开票金额")
    private BigDecimal invoiceAmount;

    @ApiModelProperty("开票日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date invoiceDate;

    @ApiModelProperty("发票附件")
    private List<DinasUrlRQ> urlList;
}

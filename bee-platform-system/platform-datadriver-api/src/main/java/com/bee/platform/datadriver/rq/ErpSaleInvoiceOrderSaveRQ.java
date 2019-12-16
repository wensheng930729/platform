package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName ErpSaleInvoiceOrderSaveRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/1$ 14:39$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售发票保存信息")
public class ErpSaleInvoiceOrderSaveRQ implements Serializable {

    private static final long serialVersionUID = 135032485276563714L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("发票编号")
    private String code;

    @ApiModelProperty("销售单id")
    private Integer saleOrder;

    @ApiModelProperty("销售单编号")
    private String saleOrderNo;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("客户id")
    private Integer customer;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("开票日期")
    private Date invoiceDate;

    @ApiModelProperty("备注")
    private String remark;
}

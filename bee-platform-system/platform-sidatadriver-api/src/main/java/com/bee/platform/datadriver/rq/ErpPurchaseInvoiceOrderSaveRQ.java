package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName ErpPurchaseInvoiceOrderSaveRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/1$ 14:42$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购发票保存信息")
public class ErpPurchaseInvoiceOrderSaveRQ implements Serializable {

    private static final long serialVersionUID = -5166187498803067573L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("发票编号")
    @NotEmpty(message = "发票编号不能为空")
    private String code;

    @ApiModelProperty("采购单id")
    @NotNull(message = "采购单id不能为空")
    private Integer purchaseOrder;

    @ApiModelProperty("采购单编号")
    @NotEmpty(message = "采购单编号不能为空")
    private String purchaseOrderNo;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer company;

    @ApiModelProperty("公司名称")
    @NotEmpty(message = "公司名称不能为空")
    private String companyName;

    @ApiModelProperty("客户id")
    private Integer supplier;

    @ApiModelProperty("客户名称")
    private String supplyName;

    @ApiModelProperty("开票日期")
    private Date invoiceDate;

    @ApiModelProperty("备注")
    private String remark;
}

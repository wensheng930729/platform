package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购发票保存信息返回的数据")
public class ErpPurchaseInvoiceOrderSaveDTO implements Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = -5162191101873459093L;

	@ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("发票编号")
    private String code;

    @ApiModelProperty("发票编号")
    private Integer detailId;

    @ApiModelProperty("采购单id")
    private Integer purchaseOrder;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("客户id")
    private Integer supplier;

    @ApiModelProperty("客户名称")
    private String supplyName;

    @ApiModelProperty("开票日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date invoiceDate;

    @ApiModelProperty("备注")
    private String remark;
}

package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author xin.huang
 * @description 销售发票列表查询请求信息
 * @date 2019/8/15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售发票列表查询请求信息")
public class SaleInvoiceListRQ implements Serializable {
    private static final long serialVersionUID = 6266443622273645293L;

    @ApiModelProperty("公司id")
    private Integer companyId;

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

    @ApiModelProperty("开票开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date invoiceDateStart;

    @ApiModelProperty("开票结束日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date invoiceDateEnd;
}

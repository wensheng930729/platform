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
 * @author dell
 * @version 1.0.0
 * @ClassName DinasInspectionGoodsSaveRQ
 * @Description 功能描述
 * @Date 2019/8/13 17:08
 **/
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("砂石验货磅单修改请求参数")
public class DinasInspectionGoodsSearchDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("采购合同id")
    private Integer purchaseOrderId;

    @ApiModelProperty("采购合同编号")
    private String purchaseOrderCode;

    @ApiModelProperty("供应商id")
    private Integer supplierId;

    @ApiModelProperty("供应商名称")
    private String supplierName;

    @ApiModelProperty("销售合同id")
    private Integer saleOrderId;

    @ApiModelProperty("销售合同编号")
    private String saleOrderCode;

    @ApiModelProperty("订货商id")
    private Integer buyerId;

    @ApiModelProperty("订货商名称")
    private String buyerName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("数量")
    private BigDecimal num;

    @ApiModelProperty("验货日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date inspectionDate;

    @ApiModelProperty("结算状态（0未结算 1或2已结算）")
    private Integer status;

    @ApiModelProperty("附件地址")
    private String url;


}

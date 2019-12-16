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
 * <p>
 * 采购结算表
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("采购结算列表返回信息")
public class DinasPurchaseSettlementSearchDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("验货磅单id")
    private Integer inspectionOrderId;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("采购合同id")
    private Integer purchaseOrderId;

    @ApiModelProperty("采购合同编号")
    private String purchaseOrderCode;

    @ApiModelProperty("供应商id")
    private Integer supplierId;

    @ApiModelProperty("供应商名称")
    private String supplierName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("不含税单价")
    private BigDecimal price;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPrice;

    @ApiModelProperty("数量")
    private BigDecimal num;

    @ApiModelProperty("总计不含税单价")
    private BigDecimal sumPrice;

    @ApiModelProperty("总计含税单价")
    private BigDecimal sumTaxPrice;

    @ApiModelProperty("结算单价")
    private BigDecimal settlementUnitPrice;

    @ApiModelProperty("结算总价")
    private BigDecimal settlementSumPrice;

    @ApiModelProperty("验货日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date inspectionDate;

    @ApiModelProperty("结算状态（0未结算 1已结算）")
    private Integer status;

    @ApiModelProperty("结算日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date settlementDate;





}

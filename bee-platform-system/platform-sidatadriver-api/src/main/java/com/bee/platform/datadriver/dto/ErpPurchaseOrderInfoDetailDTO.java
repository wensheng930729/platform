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
 * @ClassName ErpPurchaseOrderInfoDetailDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/30$ 19:10$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("合同号查询采购订单明细信息")
public class ErpPurchaseOrderInfoDetailDTO implements Serializable {

    private static final long serialVersionUID = 7941760715156015880L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("采购单编号")
    private String contractNo;

    @ApiModelProperty("采公司id")
    private Integer company;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("采购地")
    private String place;

    @ApiModelProperty("采购方式：0现货 1期货")
    private Integer purchaseMethod;

    @ApiModelProperty("供应商id")
    private Integer supplier;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("合同签订日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractTime;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("含税单价")
    private BigDecimal taxPrice;

    @ApiModelProperty("合同质量要求")
    private String requirement;

    @ApiModelProperty("进厂湿重")
    private BigDecimal totalWetWeight;

    @ApiModelProperty("扣水")
    private BigDecimal totalWaterRate;

    @ApiModelProperty("进厂干吨(收货重量)")
    private BigDecimal totalNum;

    @ApiModelProperty("水分率")
    private BigDecimal waterRate;

    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("创建人id")
    private Integer purchaseUserId;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产品批次名称")
    private String batchName;
}

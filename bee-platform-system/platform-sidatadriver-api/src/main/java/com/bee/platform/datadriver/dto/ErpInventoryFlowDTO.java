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
 * @author dell
 * @version 1.0.0
 * @ClassName ErpInventoryFlowDTO
 * @Description 功能描述
 * @Date 2019/5/31 13:52
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存流水返回信息")
public class ErpInventoryFlowDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("采购订单号")
    private String purchasingOder;

    @ApiModelProperty("入库单号")
    private String warehousingOder;

    @ApiModelProperty("入库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptDate;

    @ApiModelProperty("供应商")
    private String supplyName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;

    @ApiModelProperty("收货数量")
    private BigDecimal receiptNum;

    @ApiModelProperty("仓库名称")
    private String storeHouseName;

    @ApiModelProperty("状态(0,已保存，1已确认)")
    private Integer state;

    @ApiModelProperty("料批名称")
    private String materialBatchName;

    @ApiModelProperty("出库单号")
    private String outOfStockOrder;

    @ApiModelProperty("出库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date outOfStockDate;

    @ApiModelProperty("炉号")
    private String furnaceNumber;

    @ApiModelProperty("班次")
    private String workShift;

    @ApiModelProperty("出库数量")
    private BigDecimal outOfStockNum;

    @ApiModelProperty("入库数量")
    private BigDecimal warehousingNum;

    @ApiModelProperty("发货数量")
    private BigDecimal sendNum;

    @ApiModelProperty("销售订单号")
    private String saleOrder;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("计量单位")
    private String unit;









}

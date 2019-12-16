package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
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
 * @ClassName ErpRepositoryReceiptRawInSearchDTO
 * @Description 功能描述
 * @Date 2019/6/7 16:00
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("原料入库条件搜索返回信息列表11")
@JsonInclude
public class ErpRepositoryReceiptRawInSearchDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String  companyName;

    @ApiModelProperty("采购订单号")
    private String purchaseCode;

    @ApiModelProperty("入库单号")
    private String inCode;

    @ApiModelProperty("入库日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date  receiptDate;

    @ApiModelProperty("供应商")
    private String supplier;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("收货数量")
    private BigDecimal num;

    @ApiModelProperty("确认状态 0未确认 1已确认")
    private Integer state;

    @ApiModelProperty("产成品id")
    private Integer productId;

    @ApiModelProperty("产成品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;

    @ApiModelProperty("单位")
    private String unit;

}

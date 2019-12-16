package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Classname ErpPurchaseGoodsDetailDTO
 * @Description 条件查询货单返回信息
 * @Date 2019/5/30 14:24
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "条件查询货单返回信息")
public class ErpPurchaseGoodsDetailDTO implements Serializable {
    private static final long serialVersionUID = 6811246019885095317L;

    @ApiModelProperty("货单明细id")
    private Integer id;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("订单号")
    private String relatedOrder;

    @ApiModelProperty("库单号")
    private String code;

    @ApiModelProperty("记录日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date recordTime;

    @ApiModelProperty("客户名称")
    private String customName;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("货品数量")
    private BigDecimal num;

    @ApiModelProperty("状态")
    private Integer state;
}

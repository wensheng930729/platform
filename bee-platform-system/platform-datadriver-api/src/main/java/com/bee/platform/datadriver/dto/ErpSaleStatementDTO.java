package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname ErpSaleStatementDTO
 * @Description 销售结算详情返回信息
 * @Date 2019/5/31 17:19
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "销售结算详情返回信息")
public class ErpSaleStatementDTO implements Serializable {
    private static final long serialVersionUID = 1951815083673860703L;
    @ApiModelProperty("结算单号")
    private String code;

    @ApiModelProperty("销售订单号")
    private String saleOrder;

    @ApiModelProperty("销售订单id")
    private Integer saleOrderId;

    @ApiModelProperty("状态")
    private Integer state;

    @ApiModelProperty("销售结算单明细详情返回信息")
    private List<ErpSaleStatementDetailDTO> saleStatementDetailList;



}

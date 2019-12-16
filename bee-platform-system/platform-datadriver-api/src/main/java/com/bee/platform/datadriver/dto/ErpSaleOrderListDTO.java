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
 * @Classname ErpSaleOrderListDTO
 * @Description 销售订单列表返回消息
 * @Date 2019/6/10 15:47
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("销售订单列表返回消息")
public class ErpSaleOrderListDTO implements Serializable {
    private static final long serialVersionUID = -6292365988054963304L;

    @ApiModelProperty("销售订单明细id")
    private Integer id;

    @ApiModelProperty("销售订单id")
    private Integer orderId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("合同编号")
    private String contractNo;

    @ApiModelProperty("签订日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDate;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("合同金额")
    private BigDecimal amount;

    @ApiModelProperty("发货状态 0未收货 1部分收货 2全部收货")
    private Integer deliveryState;

    @ApiModelProperty("发票状态 0未开票 1已开票")
    private Integer invoceState;

    @ApiModelProperty("结算状态 0未结算 1已结算")
    private Integer accountState;

    @ApiModelProperty("销售员id")
    private Integer saleUserId;

    @ApiModelProperty("销售员姓名")
    private String saleUserName;
}

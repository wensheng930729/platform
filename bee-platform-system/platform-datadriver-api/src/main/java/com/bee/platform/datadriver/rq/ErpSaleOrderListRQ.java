package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname ErpSaleOrderListRQ
 * @Description 销售订单列表查询传参
 * @Date 2019/6/10 15:32
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("销售订单列表查询传参")
public class ErpSaleOrderListRQ implements Serializable {
    private static final long serialVersionUID = -3577887529014237785L;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("销售订单号")
    private String contractNo;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("发货状态 0未收货 1部分收货 2全部收货")
    private Integer deliveryState;

    @ApiModelProperty("发票状态 0未开票 1已开票")
    private Integer invoiceState;

    @ApiModelProperty("结算状态 0未结算 1已结算")
    private Integer accountState;

    @ApiModelProperty("合同日期开始时间")
    private String startTime;

    @ApiModelProperty("合同日期结束时间")
    private String endTime;

    @ApiModelProperty("当前用户所在企业及子企业id列表")
    private List<Integer> enterpriseIdList;
}

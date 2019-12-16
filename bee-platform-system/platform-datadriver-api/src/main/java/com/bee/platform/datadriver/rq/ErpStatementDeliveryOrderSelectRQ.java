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
 * @Classname ErpStatementDeliveryOrderSelectRQ
 * @Description 销售发货单查询请求信息
 * @Date 2019/5/31 11:19
 * @Author xin.huang
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("销售发货单查询请求信息")
public class ErpStatementDeliveryOrderSelectRQ implements Serializable{
    private static final long serialVersionUID = -4802721562456005151L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("销售订单号")
    private String relatedOrder;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("出库开始日期")
    private String receiptStartDate;

    @ApiModelProperty("出库结束日期")
    private String receiptEndDate;

    @ApiModelProperty("发货单状态")
    private Integer state;

    @ApiModelProperty("出库单号")
    private String code;

    @ApiModelProperty("企业id列表")
    private List<Integer> enterpriseIdList;
}

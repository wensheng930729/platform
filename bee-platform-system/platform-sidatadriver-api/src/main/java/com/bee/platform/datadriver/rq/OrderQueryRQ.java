package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName orderQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 17:41$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("订单查询请求信息")
public class OrderQueryRQ implements Serializable {

    private static final long serialVersionUID = 6949583032929431006L;

    @ApiModelProperty("订单id")
    private String orderId;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("供应商")
    private String supplyCompany;

    @ApiModelProperty("产品")
    private String product;

    @ApiModelProperty("开始时间")
    private String createStartTime;

    @ApiModelProperty("截止时间")
    private String createEndTime;

    @ApiModelProperty("收货状态(0未收货 1部分收货 2全部收货)")
    private String receiveState;

    @ApiModelProperty("结算状态(0未结算 1已结算)")
    private String accountState;

    @ApiModelProperty("发票状态(0未开票 1已开票)")
    private String invoiceState;

    //@ApiModelProperty("企业id-前端不传")
    //private List<Integer> list;
}

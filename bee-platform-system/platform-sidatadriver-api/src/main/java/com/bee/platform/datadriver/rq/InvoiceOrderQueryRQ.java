package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName InvoiceOrderQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/1$ 13:19$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("订单发票请求信息")
public class InvoiceOrderQueryRQ implements Serializable {
    private static final long serialVersionUID = -2087107335516203864L;
    @ApiModelProperty("订单id")
    private String orderId;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("供应商/客户")
    private String scCompany;

    @ApiModelProperty("产品")
    private String product;

    @ApiModelProperty("开始时间")
    private String createStartTime;

    @ApiModelProperty("截止时间")
    private String createEndTime;

    @ApiModelProperty("状态(0已保存 1已确认)")
    private Integer state;

    /*@ApiModelProperty("企业id-前端不传")
    private List<Integer> list;*/
}

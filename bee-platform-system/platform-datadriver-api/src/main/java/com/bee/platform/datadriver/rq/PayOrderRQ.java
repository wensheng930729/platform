package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName PayOrderRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/30$ 20:59$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("付款单查询请求信息")
public class PayOrderRQ implements Serializable {

    private static final long serialVersionUID = -4828298399512756369L;

    @ApiModelProperty("采购订单号")
    private String purchaseOrderNo;

    @ApiModelProperty("入库单号")
    private String code;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("供应商名称")
    private String supplyName;

    @ApiModelProperty("开始时间")
    private String payStartdate;

    @ApiModelProperty("截止时间")
    private String payEnddate;

}

package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName ReceiptDetailRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/31$ 16:33$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("根据订单查询收发货请求信息")
public class ReceiptDetailRQ implements Serializable {

    private static final long serialVersionUID = 8484279289458295590L;

    @ApiModelProperty("业务类型")
    private String businessType;

    @ApiModelProperty("订单id")
    private String orderId;

}

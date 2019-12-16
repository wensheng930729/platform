package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName OrderStatusCountDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/29$ 17:11$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("订单状态统计信息")
public class OrderStatusCountDTO implements Serializable {

    private static final long serialVersionUID = -9051339004801836342L;

    @ApiModelProperty("待发货")
    private Integer deliveryCount;

    @ApiModelProperty("待结算")
    private Integer accountCount;

    @ApiModelProperty("待收款")
    private Integer collectionCount;
}

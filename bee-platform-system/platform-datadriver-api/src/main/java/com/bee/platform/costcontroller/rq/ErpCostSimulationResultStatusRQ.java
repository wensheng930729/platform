package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 料批模拟请求信息
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("料批模拟计算结果状态更新请求信息")
public class ErpCostSimulationResultStatusRQ implements Serializable {

    private static final long serialVersionUID = -6882638762069647689L;

    @ApiModelProperty("料批模拟id")
    private Integer simulationId;

    @ApiModelProperty("计算结果id")
    private Integer id;

    @ApiModelProperty("计算结果状态")
    private Integer status;
}

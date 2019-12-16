package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * 料批模拟请求信息
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("料批模拟请求信息")
public class ErpCostMaterialSimulationRQ implements Serializable {

    private static final long serialVersionUID = 9019523980201630069L;

    @ApiModelProperty("料批模拟id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("料批名称")
    private String name;

    @ApiModelProperty("成本配置id")
    private Integer allocationId;

    @ApiModelProperty("erpc成本配置添加的rq")
    private ErpCostAllocationAddRQ allocation;

    @ApiModelProperty("料批模拟原料成分请求信息")
    private List<ErpCostMaterialSimulationElementRQ> elementList;

    @ApiModelProperty("料批模拟计算结果请求信息")
    private List<ErpCostMaterialSimulationResultRQ> resultList;

}

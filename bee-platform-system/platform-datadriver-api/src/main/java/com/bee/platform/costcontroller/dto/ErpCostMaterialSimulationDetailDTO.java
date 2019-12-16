package com.bee.platform.costcontroller.dto;

import com.bee.platform.costcontroller.rq.ErpCostAllocationAddRQ;
import com.bee.platform.costcontroller.rq.ErpCostMaterialSimulationElementRQ;
import com.bee.platform.costcontroller.rq.ErpCostMaterialSimulationResultRQ;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * 料批模拟详情返回信息
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批模拟详情返回信息")
public class ErpCostMaterialSimulationDetailDTO implements Serializable {
    private static final long serialVersionUID = -9069627488704046231L;

    @ApiModelProperty("料批模拟id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("料批名称")
    private String name;

    @ApiModelProperty("成本配置id")
    private Integer allocationId;

    @ApiModelProperty("成本配置名称")
    private String allocationName;

    @ApiModelProperty("成本配置详情")
    private String allocationInfo;

    @ApiModelProperty("创建人")
    private Integer creator;

    @ApiModelProperty("erpc成本配置详情DTO")
    private ErpCostAllocationDetailDTO allocation;

    @ApiModelProperty("料批模拟原料成分请求信息")
    private List<ErpCostMaterialSimulationElementDTO> elementList;

    @ApiModelProperty("料批模拟原料成分统计信息")
    private ErpCostMaterialSimulationElementDTO statistic;

    @ApiModelProperty("料批模拟计算结果请求信息")
    private List<ErpCostMaterialSimulationResultDTO> resultList;
}

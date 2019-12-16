package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * 料批模拟原料成分请求信息
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("料批模拟列表查询请求信息")
public class ErpCostMaterialSimulationListRQ implements Serializable {
    private static final long serialVersionUID = -346105991020960465L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("料批id")
    private Integer id;

    @ApiModelProperty("料批名称")
    private String name;

    @ApiModelProperty("成本配置id")
    private Integer allocationId;

    private List<Integer> enterpriseIdList;

}

package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("根据采购合同id查询化验单详情")
public class ErpTestReportInfoRQ implements Serializable{
    private static final long serialVersionUID = 68829977915344770L;

    @ApiModelProperty("采购合同id")
    private Integer orderId;

    @ApiModelProperty("产品批次id")
    private Integer batchId;
}

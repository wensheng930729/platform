package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购结算单返回详细信息")
public class ErpPurchaseStatementDTO implements Serializable {
    private static final long serialVersionUID = 8603091259150930717L;

    @ApiModelProperty("结算单id")
    private Integer id;

    @ApiModelProperty("结算单号")
    private String code;

    @ApiModelProperty("状态")
    private Integer state;

    @ApiModelProperty("产品批次名称")
    private String batchName;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("采购结算单返回详细信息")
    private List<ErpPurchaseStatementDetailDTO> purchaseStmtList;

}

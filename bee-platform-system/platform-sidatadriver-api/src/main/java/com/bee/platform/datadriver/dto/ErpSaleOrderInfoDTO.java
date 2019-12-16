package com.bee.platform.datadriver.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("销售订单产品批次详情返回信息")
public class ErpSaleOrderInfoDTO implements Serializable {
    private static final long serialVersionUID = -2825880484084671770L;

    @ApiModelProperty("查询销售订单返回参数")
    private List<ErpSaleOrderQueryDTO> saleOrderList;

    @ApiModelProperty("产品批次列表")
    private List<ErpProductBatchListDTO> productBatchList;
}

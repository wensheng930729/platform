package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpProductAndBatchListDTO
 * @Description 功能描述
 * @Date 2019/7/22 16:51
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品拼批次列表返回信息")
public class ErpProductAndBatchListDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产品名称拼批次")
    private String productAndBatch;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("单位")
    private String unit;
}

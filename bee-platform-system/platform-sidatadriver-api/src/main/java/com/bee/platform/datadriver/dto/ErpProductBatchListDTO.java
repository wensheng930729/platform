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
 * @ClassName ErpProductBatchListDTO
 * @Description 功能描述
 * @Date 2019/7/16 10:44
 **/
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("产品批次列表")
public class ErpProductBatchListDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("批次名称")
    private String batchName;

}

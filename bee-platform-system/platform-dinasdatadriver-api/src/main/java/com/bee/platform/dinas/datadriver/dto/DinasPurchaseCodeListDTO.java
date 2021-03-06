package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName DinasPurchaseCodeListDTO
 * @Description 功能描述
 * @Date 2019/8/14 10:04
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购合同编号列表返回信息")
public class DinasPurchaseCodeListDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("采购合同id")
    private Integer purchaseOrderId;

    @ApiModelProperty("采购合同编号")
    private String purchaseOrderCode;


}

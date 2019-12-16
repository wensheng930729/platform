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
 * @ClassName ErpPurchaseListDTO
 * @Description 功能描述
 * @Date 2019/6/7 20:28
 **/


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("采购订单列表")
public class ErpPurchaseListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("编号")
    private String contractNo;

}

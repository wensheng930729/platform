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
 * @ClassName DinasProductList2DTO
 * @Description 功能描述
 * @Date 2019/8/14 10:07
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品规格列表返回信息")
public class DinasProductSpecListDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}

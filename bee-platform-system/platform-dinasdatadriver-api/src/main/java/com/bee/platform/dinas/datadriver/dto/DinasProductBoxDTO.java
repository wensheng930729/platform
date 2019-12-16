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
 * @ClassName DinasProductBoxDTO
 * @Description 功能描述
 * @Date 2019/8/14 10:07
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品列表返回信息")
public class DinasProductBoxDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品单位")
    private String unit;

}

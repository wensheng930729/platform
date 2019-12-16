package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品类别
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "erp产品检测属性DTO")
public class ErpProductCheckItemsDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品类别名称")
    private String name;

    @ApiModelProperty("主属性 ? 1:0")
    private Integer show;

}

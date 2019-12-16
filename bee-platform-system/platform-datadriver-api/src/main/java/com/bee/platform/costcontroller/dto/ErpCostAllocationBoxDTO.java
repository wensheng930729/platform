package com.bee.platform.costcontroller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * erp成本小工具-成本配置
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "erp成本配置下拉框列表DTO")
public class ErpCostAllocationBoxDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("配置id")
    private Integer id;

    @ApiModelProperty("成本配置名称")
    private String name;

}

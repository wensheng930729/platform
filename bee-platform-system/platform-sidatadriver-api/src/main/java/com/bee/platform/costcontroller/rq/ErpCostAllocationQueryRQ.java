package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

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
@ApiModel(value = "erp成本配置列表查询的rq")
public class ErpCostAllocationQueryRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("成本配置名称")
    private String name;

    @ApiModelProperty("状态")
    private Integer status;

}

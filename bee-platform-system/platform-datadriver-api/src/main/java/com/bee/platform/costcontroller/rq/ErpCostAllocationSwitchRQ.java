package com.bee.platform.costcontroller.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
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
@ApiModel(value = "erpc成本配置切换状态的rq")
public class ErpCostAllocationSwitchRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("配置的id")
    @NotNull(message = "配置的id不能为空")
    private Integer id;

    @ApiModelProperty("状态")
    @NotNull(message = "状态不能为空")
    private Integer status;

}

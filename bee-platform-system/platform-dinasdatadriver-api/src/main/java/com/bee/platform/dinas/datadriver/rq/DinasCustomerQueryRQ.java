package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName DinasProductAndSpecIdRQ
 * @Description 砂石客户add的RQ
 * @Date 2019-8-13
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石客户add的RQ")
public class DinasCustomerQueryRQ implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("是否启用-0禁用1启用")
    private Integer status;

    @ApiModelProperty("类型0采购商1销售商")
    @NotNull(message = "客户类型不能为空")
    private Integer type;

}

package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName DinasCustomerDetailDTO
 * @Description DinasCustomerDetailDTO
 * @Date 2019-8-14
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石客户列表的DTO")
public class DinasCustomerDetailDTO implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("客户id")
    private Integer id;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("是否启用-0禁用1启用")
    private Integer status;

}

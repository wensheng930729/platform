package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName DinasCustomerListDTO
 * @Description DinasCustomerListDTO
 * @Date 2019-8-14
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石客户列表的DTO")
public class DinasCustomerBoxDTO implements Serializable {

    private static final long serialVersionUID = -1;

    @ApiModelProperty("客户id")
    private Integer id;

    @ApiModelProperty("客户名称")
    private String customerName;

}

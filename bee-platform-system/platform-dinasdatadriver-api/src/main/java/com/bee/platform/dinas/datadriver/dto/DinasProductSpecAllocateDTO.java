package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName DinasProductSpecAllocateDTO
 * @Description DinasProductSpecAllocateDTO
 * @Date 2019-8-14
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石产品规格分配的DTO")
public class DinasProductSpecAllocateDTO implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("客户id")
    private Integer customerId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("规格id")
    private Integer specId;

    @ApiModelProperty("规格名称")
    private String specName;


}

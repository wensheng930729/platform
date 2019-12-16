package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName DinasProductSpecBoxDTO
 * @Description 砂石产品规格box的DTO
 * @Date 2019-8-14
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石产品规格box的DTO")
public class DinasProductSpecBoxDTO implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("规格id")
    private Integer id;

    @ApiModelProperty("规格名称")
    private String specName;




}

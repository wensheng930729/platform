package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName ErpProductBoxDTO
 * @Description erp产品下拉列表返回DTO
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "erp产品下拉列表返回DTO")
@JsonInclude
public class ErpProductBoxDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("产品名称")
    private String name;

    /**
     * 单位
     */
    @ApiModelProperty("单位")
    private String unit;




}

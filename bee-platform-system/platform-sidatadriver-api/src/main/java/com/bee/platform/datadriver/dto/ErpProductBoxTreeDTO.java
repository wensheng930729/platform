package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author liang.li
 * @ClassName ErpProductBoxTreeDTO
 * @Description erp产品下拉列表返回DTO
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "erp产品下拉列表树形返回DTO")
@JsonInclude
public class ErpProductBoxTreeDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("产品分类")
    private Integer category;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("产品批次")
    private List<ErpProductBatchDTO> batchList;


}

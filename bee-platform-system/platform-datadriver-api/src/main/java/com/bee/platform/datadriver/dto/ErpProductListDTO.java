package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName ErpProductListDTO
 * @Description erp产品列表dto
 * @Date 2019-5-28
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel(value = "erp产品列表的返回DTO")
public class ErpProductListDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("产品编码")
    private String code;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("产品类别")
    private Integer category;

    @ApiModelProperty("产品类别名称")
    private String categoryName;


    @ApiModelProperty("启用批次，1-启用，0-不启用")
    private Integer enableBatch;

    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;


}

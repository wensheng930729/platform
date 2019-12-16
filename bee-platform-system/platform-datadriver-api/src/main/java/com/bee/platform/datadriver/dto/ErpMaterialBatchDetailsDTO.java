package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpMaterialBatchDetailsDTO
 * @Description 功能描述
 * @Date 2019/6/2 19:44
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("根据料批主单id查询详情产品信息")
@JsonInclude
public class ErpMaterialBatchDetailsDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("产品名称")
    private String productName;
    @ApiModelProperty("计量单位")
    private String unit;



}

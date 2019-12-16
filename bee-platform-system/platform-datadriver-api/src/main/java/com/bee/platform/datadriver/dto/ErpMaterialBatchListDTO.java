package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpMaterialBatchListDTO
 * @Description 功能描述
 * @Date 2019/6/2 15:05
 **/


@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("料批下拉列表返回信息")
@JsonInclude
public class ErpMaterialBatchListDTO {

    @ApiModelProperty("id")
    private  Integer id;

    @ApiModelProperty("料批名称")
    private String materialBatchName;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("计量单位")
    private String unit;

}

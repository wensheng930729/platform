package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ProductAndOtherDTO
 * @Description 功能描述
 * @Date 2019/7/23 13:58
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("成品入库根据化验单id查询产品等信息返回信息")
public class ProductAndOtherDTO implements Serializable {
    private static final long serialVersionUID = 2708325555826364889L;
    /**
     * 出炉号id、炉号名称、班次、产成品id、产品名称、产品批次id、产品拼批次 、单位
     */

    @ApiModelProperty("炉号id")
    private Integer furnaceId;

    @ApiModelProperty("炉号名称")
    private String furnaceName;

    @ApiModelProperty("班次")
    private String classes;

    @ApiModelProperty("产成品id")
    private Integer productId;

    @ApiModelProperty("产成品名称")
    private String productName;

    @ApiModelProperty("产成品批次id")
    private Integer productBatchId;

    @ApiModelProperty("产成品拼批次")
    private String productAndBatch;

    @ApiModelProperty("单位")
    private String unit;

}

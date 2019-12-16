package com.bee.platform.datadriver.dto;


import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
/**
 * <p>
 * 库存盘点主单表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存盘点搜索返回信息")
@JsonInclude
public class ErpStockCheckSearchListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("确认状态（0已保存，1已确认）")
    private Integer state;

    @ApiModelProperty("产品名称")
    private String productName;


    @ApiModelProperty("仓库")
    private String storehouse;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("化验单")
    private String testCode;

    @ApiModelProperty("理论数量")
    private BigDecimal expectNumber;

    @ApiModelProperty("实际数量")
    private BigDecimal realNumber;






}

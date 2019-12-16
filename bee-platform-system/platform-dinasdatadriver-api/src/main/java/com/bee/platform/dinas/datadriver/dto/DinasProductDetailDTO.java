package com.bee.platform.dinas.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author liang.li
 * @ClassName DinasProductList2DTO
 * @Description 砂石产品list的DTO
 * @Date 2019-8-14
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石产品list的DTO")
public class DinasProductDetailDTO implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("产品id")
    private Integer id;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("是否启用-0禁用1启用")
    private Integer status;

    @ApiModelProperty("产品批次信息")
    private List<DinasProductSpecBoxDTO> specList;


}

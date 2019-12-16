package com.bee.platform.costcontroller.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * 料批模拟返回信息
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批模拟返回信息")
public class ErpCostMaterialSimulationDTO implements Serializable {
    private static final long serialVersionUID = -4487128829827980482L;

    @ApiModelProperty("料批模拟id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("料批名称")
    private String name;

    @ApiModelProperty("成本配置id")
    private Integer allocationId;

    @ApiModelProperty("成本配置详情(json数据)")
    private String allocationInfo;

    @ApiModelProperty("创建人id")
    private Integer creator;

    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTime;

    @ApiModelProperty("更新时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTime;

}

package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("仓库档案对象")
public class ErprepositoryDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("仓库档案id")
    private Integer id;

    @ApiModelProperty("名称")
    private String name;

    @ApiModelProperty("状态")
    private Integer status;

    @ApiModelProperty("所属企业id")
    private Integer orgId;
}

package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("仓库档案列表DTO")
public class ErpRepositoryListDTO implements Serializable{

	private static final long serialVersionUID = -8660403801235472857L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("名称")
    private String name;

    @ApiModelProperty("所属企业id")
    private Integer orgId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("仓库类型")
    private String type;

    @ApiModelProperty("状态")
    private Integer status;
}

package com.bee.platform.datadriver.rq;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("仓库档案添加信息")
public class ErpRepositoryAddRQ implements Serializable{

    private static final long serialVersionUID = 1L;

	 /**
     * 名称
     */
     @ApiModelProperty("名称")
    private String name;
    /**
     * 状态
     */
    @ApiModelProperty("状态")
    private Integer status;
    /**
     * 所属企业id
     */
    @ApiModelProperty("所属企业id")
    private Integer orgId;
}

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
@ApiModel("仓库档案编辑信息")
public class ErpRepositoryUpdataRQ implements Serializable{
    private static final long serialVersionUID = 1L;
	/**
     * 名称
     */
    @ApiModelProperty("名称") 
    private String name;
    /**
     * 仓库id
     */
    @ApiModelProperty("id")
    private Integer id;
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
    /**
     * 删除状态
     */
    @ApiModelProperty("删除状态")
    private Integer deleted;
}

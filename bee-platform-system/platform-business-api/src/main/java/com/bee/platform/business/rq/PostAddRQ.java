package com.bee.platform.business.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "职位编辑rq")
public class PostAddRQ implements Serializable {

    private static final long serialVersionUID = 6896254615540896655L;

    @ApiModelProperty("职位id 编辑传")
    private Integer id;

    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("职位名")
    private String name;

    @ApiModelProperty("职位描述")
    private String description;
}

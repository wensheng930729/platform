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
@ApiModel(value = "部门编辑rq")
public class DepartmentAddRQ implements Serializable {

    private static final long serialVersionUID = -2093770434481962916L;

    @ApiModelProperty("上级部门id 当前部门为一级部门时不传值")
    private Integer treeId;

    @ApiModelProperty("部门id 编辑传")
    private Integer departmentId;

    @ApiModelProperty("部门层级 1 2 3...")
    private Integer level;

    @ApiModelProperty("部门名")
    private String name;

    @ApiModelProperty("部门描述")
    private String description;
}

package com.bee.platform.business.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description: 部门层级
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("部门dto")
public class DepartmentDTO implements Serializable{

    private static final long serialVersionUID = -2307447489329825140L;

    @ApiModelProperty("部门id")
    private Integer id;
    
    @ApiModelProperty("部门名称")
    private String name;

    @ApiModelProperty("父id")
    private Integer treeId;

    @ApiModelProperty("部门层级")
    private Integer level;

}

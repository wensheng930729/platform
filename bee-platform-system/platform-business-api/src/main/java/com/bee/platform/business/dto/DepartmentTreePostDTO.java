package com.bee.platform.business.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @description: 部门层级树 职位用
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("部门层级树 职位用")
public class DepartmentTreePostDTO implements Serializable{

    private static final long serialVersionUID = 3692505175855455384L;

    @ApiModelProperty("部门id")
    private Integer id;
    
    @ApiModelProperty("部门名称")
    private String name;

    @ApiModelProperty("父id")
    private Integer treeId;

    @ApiModelProperty("部门层级")
    private Integer level;

    private List<DepartmentTreePostDTO> subList;

}

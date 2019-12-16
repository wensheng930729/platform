package com.bee.platform.user.dto;

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
 * @description: 部门树dto
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("部门树dto")
public class DepartmentTreeDTO implements Serializable{

    private static final long serialVersionUID = -2307447489329825140L;

    @ApiModelProperty("部门id")
    private Integer id;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("部门名称")
    private String name;

    @ApiModelProperty("父id")
    private Integer treeId;

    @ApiModelProperty("部门层级")
    private Integer level;

    @ApiModelProperty("职能描述")
    private String description;

    @ApiModelProperty("下属部门")
    private List<DepartmentTreeDTO> children;
}

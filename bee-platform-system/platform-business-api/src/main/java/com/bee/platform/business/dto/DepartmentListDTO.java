package com.bee.platform.business.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 部门列表返回数据
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("部门列表返回数据")
public class DepartmentListDTO implements Serializable{

    private static final long serialVersionUID = 46885436496813870L;

    @ApiModelProperty("部门id")
    private Integer id;
    
    @ApiModelProperty("部门名称")
    private String name;

    @ApiModelProperty("部门人数")
    private Integer count;

    @ApiModelProperty("隶属关系")
    private String relation;

    @ApiModelProperty("职能描述")
    private String description;

}

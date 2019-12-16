package com.bee.platform.business.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 职位列表返回数据
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("职位列表返回数据")
@JsonInclude
public class PostListDTO implements Serializable{

    private static final long serialVersionUID = 4717466821252176351L;

    @ApiModelProperty("职位id")
    private Integer id;
    
    @ApiModelProperty("职位名称")
    private String name;

    @ApiModelProperty("所属关系")
    private String relation;

    @ApiModelProperty("职能描述")
    private String description;

    @ApiModelProperty("是否可删 0否 1是")
    private Integer deletable;

    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("部门层级")
    private Integer level;

}

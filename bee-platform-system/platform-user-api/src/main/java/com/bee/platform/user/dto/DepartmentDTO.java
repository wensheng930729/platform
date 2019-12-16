package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-21 15:36
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("部门相关返回数据")
public class DepartmentDTO implements Serializable {

    private static final long serialVersionUID = -3126338576612749402L;
    @ApiModelProperty("部门id")
    private Integer id;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("父id")
    private Integer treeId;

    @ApiModelProperty("部门名称")
    private String name;
}

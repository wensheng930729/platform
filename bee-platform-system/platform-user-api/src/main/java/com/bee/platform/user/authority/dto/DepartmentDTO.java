package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "部门相关的")
public class DepartmentDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("部门id")
    private Integer id;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("部门树id")
    private Integer treeId;

    @ApiModelProperty("部门名称")
    private String name;

    @ApiModelProperty("部门创建日期")
    private Date createAt;

    @ApiModelProperty("部门修改日期")
    private Date updateAt;

}

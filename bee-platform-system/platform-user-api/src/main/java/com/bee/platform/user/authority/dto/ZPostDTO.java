package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 职位表

 * </p>
 *
 * @author liliang123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "职位相关的")
public class ZPostDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("职位ID")
    private Integer id;

    @ApiModelProperty("职位名称")
    private String name;

    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("状态：0删除")
    private Integer status;

    @ApiModelProperty("职位描述")
    private String description;


}

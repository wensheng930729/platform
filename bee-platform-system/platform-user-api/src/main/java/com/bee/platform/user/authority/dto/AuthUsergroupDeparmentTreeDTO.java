package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "用户组详情DTO")
public class AuthUsergroupDeparmentTreeDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("部门id")
    private Integer departmentId;

    @ApiModelProperty("上级部门id")
    private Integer treeId;

    @ApiModelProperty("部门名称")
    private String  department;

    @ApiModelProperty("后代")
    private List<AuthUsergroupDeparmentTreeDTO> children;


}

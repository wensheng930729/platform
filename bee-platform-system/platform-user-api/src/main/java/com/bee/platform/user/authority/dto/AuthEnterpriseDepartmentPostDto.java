package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @Classname AuthEnterpriseDepartmentPostDto
 * @Description 用户所在公司部门职位信息
 * @Date 2019/5/27 16:44
 * @Author xin.huang
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户所在公司部门职位信息")
public class AuthEnterpriseDepartmentPostDto implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("归属公司id")
    private Integer enterpriseId;

    @ApiModelProperty("归属公司名称")
    private String enterpriseName;

    @ApiModelProperty("归属公司简称")
    private String enterpriseSimpleName;

    @ApiModelProperty("用户状态")
    private String status;

    @ApiModelProperty("部门Id")
    private Integer departmentId;

    @ApiModelProperty("部门名称")
    private String departmentName;

    @ApiModelProperty("职位Id")
    private Integer postId;

    @ApiModelProperty("职位名称")
    private String postName;

    @ApiModelProperty("用户类型： 1企业成员 2企业管理员")
    private Integer userType = 1;

    @ApiModelProperty("部门id列表")
    private List<Integer> departmentIds;

}

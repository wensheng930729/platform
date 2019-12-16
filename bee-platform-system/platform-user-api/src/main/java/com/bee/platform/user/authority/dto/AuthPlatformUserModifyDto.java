package com.bee.platform.user.authority.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "判断后平台用户返回数据")
public class AuthPlatformUserModifyDto implements Serializable {
    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("归属公司id")
    private Integer enterpriseId;

    @ApiModelProperty("业务id")
    private String beesrvId;

    @ApiModelProperty("归属公司名称")
    private String enterpriseName;

    @ApiModelProperty("归属公司简称")
    private String enterpriseSimpleName;

    @ApiModelProperty("部门Id")
    private Integer departmentId;

    @ApiModelProperty("部门名称")
    private String departmentName;

    @ApiModelProperty("职位Id")
    private Integer postId;

    @ApiModelProperty("职位名称")
    private String postName;

    @ApiModelProperty("手机号")
    private String phone;

    @ApiModelProperty("姓名")
    private String name;

    @ApiModelProperty("用户账号")
    private String username;

    @ApiModelProperty("用户名")
    private String nickname;

    @ApiModelProperty("头像")
    private String head;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("县级地区id")
    private String regionId;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("固话")
    private String fixtel;

    @ApiModelProperty("是否启用：1启用 0禁用")
    private Integer status;

    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTime;

    @ApiModelProperty("更新时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTime;

    @ApiModelProperty("修改人名字")
    private String updateName;

    @ApiModelProperty("企业id集合")
    private List enterpriseIds;

    @ApiModelProperty("角色类型用户类型:1application 2function 3custom 4base 5enterprise_admin  6super_admin 7 Other")
    private String roleType;

    @ApiModelProperty("用户下企业、部门、职位名称的集合")
    private List<EdpNameDTO> edpNameDTOS;
}

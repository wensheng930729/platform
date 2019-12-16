package com.bee.platform.user.authority.dto;

import com.bee.platform.common.dto.RegionDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @Classname AuthUserEnterpriseDetailDto
 * @Description 企业用户返回信息
 * @Date 2019/5/27 16:27
 * @Author xin.huang
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "企业用户返回信息")
public class AuthUserEnterpriseDetailDto implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("业务id")
    private String beesrvId;

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
    private Date createTime;

    @ApiModelProperty("更新时间")
    private Date updateTime;

    @ApiModelProperty("关联公司部门职位")
    List<AuthEnterpriseDepartmentPostDto> enterpriseDepartmentPostList;

    @ApiModelProperty("权限列表")
    List<AuthUserRoleTreeDTO> roleList;

    @ApiModelProperty("区域信息")
    RegionDTO regionInfo;

}

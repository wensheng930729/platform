package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * @Classname AuthPlatformUserRQ
 * @Description 平台用户信息传输对象
 * @Date 2019/5/20 16:55
 * @Author xin.huang
 */
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("平台用户信息传输对象")
public class AuthPlatformUserRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("手机号")
    private String phone;

    @ApiModelProperty("姓名")
    @NotEmpty(message = "姓名不能为空")
    private String name;

    @ApiModelProperty("用户账号")
    @NotEmpty(message = "用户账号不能为空")
    private String username;

    @ApiModelProperty("用户名")
    private String nickname;

    @ApiModelProperty("密码")
    private String password;

    @ApiModelProperty("头像")
    private String head;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("qq")
    private String qq;

    @ApiModelProperty("县级地区id")
    private String regionId;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("固话")
    private String fixtel;

    @ApiModelProperty("修改人")
    private Integer updateUser;

    @ApiModelProperty("当前用户所在企业")
    private Integer companyId;

    @ApiModelProperty("关联公司部门职位")
    private List<AuthPlatformUserDepartmentAndPostRQ> departmentAndPostList;

    @ApiModelProperty("企业用户关联角色功能应用请求参数")
    private List<UserRelationRoleRQ> userRoleList;


}

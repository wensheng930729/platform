package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.Digits;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("后台用户信息传输对象")
public class AuthPlatformUserAfterRQ implements Serializable {

	private static final long serialVersionUID = -7692191997797090755L;

	@ApiModelProperty("手机号")
    @Digits(integer = 11 ,fraction = 0 ,message = "输入正确的手机好号码")
    private String phone;

    @ApiModelProperty("姓名")
    private String name;

    @ApiModelProperty("用户账号")
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
    
    @ApiModelProperty("权限配置")
    private AuthUserRoleRQ authUserRoleRQ;
    
    @ApiModelProperty("用户账号说明")
    private String accountDescription;
    
    @ApiModelProperty("角色类型用户类型:1application 2function 3custom 4base 5enterprise_admin  6super_admin 7 Other")
    private Integer roleType;
    
    @ApiModelProperty("关联公司部门职位")
    private List<AuthPlatformUserDepartmentAndPostRQ> departmentAndPostList;

//    @ApiModelProperty("企业用户关联角色功能应用请求参数")
//    private List<UserRelationRoleRQ> userRoleList;
}

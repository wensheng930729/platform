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
@ApiModel("编辑后台管理中的中台用户传输对象")
public class AuthPlatformUserAfterUpdateRQ implements Serializable{

	private static final long serialVersionUID = 661482066290340699L;

		@ApiModelProperty("手机号")
	    @Digits(integer = 11 ,fraction = 0 ,message = "输入正确的手机好号码")
	    private String phone;

	    @ApiModelProperty("姓名")
	    private String name;
	    
	    @ApiModelProperty("用户id")
	    private Integer id;

	    @ApiModelProperty("用户账号")
	    @NotEmpty(message = "用户账号不能为空")
	    private String username;

	    @ApiModelProperty("用户名")
	    private String nickname;

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

		@ApiModelProperty("用户类型：0中台用户 1后台用户 2普通用户")
		private Integer userType;
		
		@ApiModelProperty("企业id")
		private Integer enterpriseId;
		
		@ApiModelProperty("账户说明")
		private String accountDescription;
		
		@ApiModelProperty("角色类型用户类型:1application 2function 3custom 4base 5enterprise_admin  6super_admin 7 other")
		private Integer roleType;
		 
	    @ApiModelProperty("关联公司部门职位")
	    private List<AuthPlatformUserDepartmentAndPostRQ> departmentAndPostList;
	    
//	    @ApiModelProperty("企业用户关联角色功能应用请求参数")
//	    private List<UserRelationRoleRQ> authUserRoleRQ;
}

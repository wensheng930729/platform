package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("中台一个用户查询返回对象")
public class AuthPlatformUserOneInDTO implements Serializable{
	 /**
	 * 
	 */
	private static final long serialVersionUID = 7646572575200583970L;

	 @ApiModelProperty("用户id")
	 private Integer id;
	 
	 @ApiModelProperty("头像")
	 private String head;
	    
	 @ApiModelProperty("手机号")
	 private String phone;
	 
	 @ApiModelProperty("用户名")
	 private String name;
	 
	 @ApiModelProperty("邮箱")
	 private String email;
	    
	 @ApiModelProperty("邮箱")
	 private String fixtel;
	    
	  
	 @ApiModelProperty("县级地区")
	 private String regionName;
	  
	 @ApiModelProperty("详细地址")
	 private String address;
	  
	 @ApiModelProperty("当前登录部门")
	 private String departmentName;
	 
	 @ApiModelProperty("当前登录企业")
	 private String enterpriseName;
	 
	 @ApiModelProperty("当前登录职位")
	 private String postName;
}

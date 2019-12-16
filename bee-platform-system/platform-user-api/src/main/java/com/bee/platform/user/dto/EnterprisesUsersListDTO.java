package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("查询企业用户信息列表返回对象")
public class EnterprisesUsersListDTO implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 8827218658037273743L;
	/**
	 * 用户ID
	 */
	private Integer userId;
	/**
	 * 	手机号
	 */
    private String phone;
    /**
     * 	姓名
     */
    private String nickname;
    /**
     * 职位
     */
    private String post;
    /**
     *	 用户账号是否被激活 状态：1表示激活
     */
    private  Integer isActive;
    /**
     * 部门id
     */
    private Integer departmentId;
    /**
     * 部门名字
     */
    private String departmentName;
    /**
     * 	邮箱
     */
    private String email;
}

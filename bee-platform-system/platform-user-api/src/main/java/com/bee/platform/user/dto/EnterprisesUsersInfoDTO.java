package com.bee.platform.user.dto;

import java.io.Serializable;
import javax.validation.constraints.Digits;
import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业用户信息请求对象")
public class EnterprisesUsersInfoDTO implements Serializable{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 2439957337111277010L;
	/**
	 * 企业用户ID
	 */
	private Integer id;
	/**
	 * 	用户ID
	 */
	private Integer userid;
	 /**
     * 	头像
     */
    private String head;

	/**
	 * 	手机号
	 */
    @Digits(integer = 11 ,fraction = 0 ,message = "输入正确的手机好号码")
    private String phone;
    
    /**
     * 	姓名
     */
    @NotNull
    private String nickname;
    
    /**
     * 	邮箱
     */
    private String email;
    
    /**
     * 	固话
     */
    private String fixtel;
    
    /**
     * 	县级地区id
     */
    private String regionid;
    
    /**
     * 	详细地址
     */
    private String address;
    
    /**
     * 	部门id
     */
    @NotNull
    private Integer departmentsid;
    
    /**
     * 	职位ID
     */
    private Integer zpostid;

}

package com.bee.platform.user.dto;

import java.io.Serializable;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@Accessors(chain = true)
public class AuthPlatformUserEnterpriseDTO implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = -5994631106382198244L;
	/**
     * 主键id
     */
   
    private Integer id;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 部门id
     */
    private Integer departmentsId;
    /**
     * 职位id
     */
    private Integer postId;
    /**
     * 状态：1启动 0禁用
     */
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;

}

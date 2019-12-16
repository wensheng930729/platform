package com.bee.platform.common.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description: 后台角色信息
 * @author: junyang.li
 * @create: 2019-04-29 15:35
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
public class MRoleInfo implements Serializable {

    
	private static final long serialVersionUID = -5980494351910455404L;
	/**
     * 角色id
     */
    private Integer roleId;
    /**
     * 角色名称
     */
    private String roleName;
    /**
     * 角色类型
     */
    private Integer roleType;
    /**
     * 创建企业
     */
    private String createCompanyId;
    /**
     * 角色状态
     */
    private Integer status;
}

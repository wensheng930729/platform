package com.bee.platform.user.dto;

import java.io.Serializable;

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
@ApiModel("企业审核中已开通的产品及角色返回对象")
public class EnterprisesAppsOpenedDTO implements Serializable {
	private static final long serialVersionUID = 1L;
	
	/**
     * 产品名称
     */
    private String appName;
    /**
     * 产品角色名称
     */
    private String appRolesName;

}

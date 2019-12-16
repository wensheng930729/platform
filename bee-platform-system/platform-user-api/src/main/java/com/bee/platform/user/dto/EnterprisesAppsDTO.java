package com.bee.platform.user.dto;

import java.io.Serializable;
import java.sql.Date;

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
@ApiModel("企业申请产品及角色返回对象")
public class EnterprisesAppsDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	private Integer id;
    /**
     * 公司名称
     */
    private String orgName;
    /**
     * 产品名称
     */
    private String appName;
    /**
     * 产品角色名称
     */
    private String appRolesName;
    /**
     * 审核状态(0: 未通过，1: 已通过, 2: 审核中)
     */
    private Integer aduitState;
    /**
     * 拒绝原因
     */
    private String rejectReason;
    /**
     * 提交时间
     */
	private Date createTime;
	/**
	 * 提交人
	 */
	private String creator;
}

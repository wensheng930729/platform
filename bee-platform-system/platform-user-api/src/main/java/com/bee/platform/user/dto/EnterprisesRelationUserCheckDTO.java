package com.bee.platform.user.dto;

import java.util.Date;


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
public class EnterprisesRelationUserCheckDTO {
	/**
	 * 企业用户关联ID
	 */
	private Integer id;
	/**
     * 手机号
     */
    private String phone;
    /**
     * 审核状态（0未通过 1已通过 2审核中）
     */
    private Integer checkStatus;
    /**
     * 申请理由
     */
    private String applyReason;
    /**
     * 拒绝原因
     */
    private String refusalReason;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 用户名
     */
    private String userName;
    /**
     * 创建时间
     */
    private Date createTime;
    
}

package com.bee.platform.user.dto;


import java.io.Serializable;

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
public class EnterprisesRelationUserCheckAmendRQ implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 7335661408520108484L;
	/**
	 * 企业关联审核ID
	 */
	private Integer id;
	/**
	 * 审核状态
	 */
	private Integer checkStatus;
	/**
	 * 拒绝原因
	 */
	private String refusalReason;
	/**
	 *  企业用户id
	 */
	private Integer userId;
}

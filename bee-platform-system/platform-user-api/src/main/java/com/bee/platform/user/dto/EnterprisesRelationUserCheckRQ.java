package com.bee.platform.user.dto;


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
public class EnterprisesRelationUserCheckRQ {


	/**
	 * 企业用户姓名或者企业用户手机号码
	 */
	private String nameOrPhone;
	/**
	 *开始时间
	 */
	private String startTime;
	/**
	 * 结束时间
	 */
	private String endTime;
	/**
	 * 企业ID
	 */
	private Integer enterpriseId;
}

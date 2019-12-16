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
public class EnterprisesUsersRQ implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 4813300653731781686L;
	/**
	 *  企业用户姓名或者企业用户手机号码
	 */
	private String nameOrPhone;
	/**
	 * 职位ID
	 */
	private Integer zpostid;
	/**
	 * 部门ID
	 */
	private Integer departmentsid;
}

package com.bee.platform.user.authority.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
@Getter
@AllArgsConstructor
public enum AuthPlatformUserType {

	 /**
     *  用户账号类型
     */
	APPLICATION(1,"application"),
	FUNCTION(2,"function"),
	CUSTOM(3,"custom"),
	BASE(4,"base"),
	ENTERPRISE_ADMIN(5,"enterprise_admin"),
	SUPER_ADMIN(6,"super_admin"),
	OTHER(7,"other");
	private Integer code;

    private String desc;

}

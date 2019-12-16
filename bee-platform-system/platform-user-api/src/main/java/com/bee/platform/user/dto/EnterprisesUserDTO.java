package com.bee.platform.user.dto;

import java.io.Serializable;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
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
@ApiModel("企业用户信息")
public class EnterprisesUserDTO implements Serializable{
	
	private static final long serialVersionUID = 1L;

	@ApiModelProperty("用户id")
	private Integer userid;
	
	@ApiModelProperty("手机号码")
    private String phone;
    
	@ApiModelProperty("姓名")
    private String nickname;

}

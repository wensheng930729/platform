package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;


import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @description: 验证用户是否在对应企业中的请求参数
 * @author: junyang.li
 * @create: 2019-01-18 18:45
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
public class UserInCompanyRQ implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 8650482150944679961L;

	@ApiModelProperty("待验证的账号")
    @NotEmpty(message = "待验证账号不能为空")
    private String username;

    @ApiModelProperty("待验证企业id")
    @NotNull(message = "待验证企业不能为空")
    private List<Integer> orgIds;
}

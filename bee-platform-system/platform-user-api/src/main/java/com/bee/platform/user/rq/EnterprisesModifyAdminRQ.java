package com.bee.platform.user.rq;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("变更管理员参数")
public class EnterprisesModifyAdminRQ implements Serializable {
	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty("申请单checkId")
    @NotNull(message = "申请单id不能为空")
	private Integer checkId;
	
    @ApiModelProperty("用户id")
    @NotNull(message = "用户id不能为空")
	private Integer userId;
	
	

}

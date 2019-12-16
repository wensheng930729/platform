package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("中台用户条件查询传输对象")
public class AuthPlatformUserSelectINRQ implements Serializable {

	private static final long serialVersionUID = 4580720447572183142L;

	@ApiModelProperty("手机号/姓名")
    private String username;

    @ApiModelProperty("公司id")
    private Integer enterpriseId;

    @ApiModelProperty("公司名称")
    private String enterpriseName;

    @ApiModelProperty("职务id")
    private Integer postId;

    @ApiModelProperty("用户状态")
    private Integer status;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;
}

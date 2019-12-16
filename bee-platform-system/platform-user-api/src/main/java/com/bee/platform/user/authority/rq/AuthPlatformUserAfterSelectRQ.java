package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("后台用户条件查询传输对象")
public class AuthPlatformUserAfterSelectRQ implements Serializable {

    @ApiModelProperty("手机号/姓名/邮箱")
    private String usernameOrPhoneOrEmail;

    @ApiModelProperty("用户状态")
    private Integer status;
    
    @ApiModelProperty("0中台用户 1后台用户 2普通用户")
    private Integer userType;
    
    @ApiModelProperty("开始时间")
    private String startTime;
    
    @ApiModelProperty("结束时间")
    private String endTime;
}

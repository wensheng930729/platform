package com.bee.platform.user.authority.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @Classname AuthPlatformUserDto
 * @Description 平台用户列表 返回数据
 * @Date 2019/5/21 9:28
 * @Author xin.huang
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户下拉列表")
public class AuthPlatformUserPullDownDto implements Serializable {

    private static final long serialVersionUID = 4874732867182112182L;

    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("姓名")
    private String name;
}

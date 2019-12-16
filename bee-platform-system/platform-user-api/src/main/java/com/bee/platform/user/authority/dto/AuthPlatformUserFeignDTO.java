package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @Classname AuthPlatformUserDto
 * @Description 平台用户列表 返回数据
 * @Date 2019/5/21 9:28
 * @Author xin.huang
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "平台用户返回数据")
public class AuthPlatformUserFeignDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("用户账号：现在是手机号")
    private String username;

    @ApiModelProperty("用户姓名")
    private String name;

    @ApiModelProperty("用户名")
    private String nickname;

    @ApiModelProperty("密码")
    private String password;

    @ApiModelProperty("头像")
    private String head;

    @ApiModelProperty("邮箱")
    private String email;

    @ApiModelProperty("qq")
    private String qq;

    @ApiModelProperty("县级地区id")
    private String regionId;

    @ApiModelProperty("详细地址")
    private String address;

    @ApiModelProperty("固话")
    private String fixtel;

    @ApiModelProperty("是否启用：1启用 0禁用")
    private Integer status;

    @ApiModelProperty("用户激活类型：0平台注册 1平台添加")
    private Integer activeType;

    @ApiModelProperty("用户类型：0中台用户 1后台用户 2普通用户 3中后台用户")
    private Integer userType;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("更新时间")
    private Date updateTime;

    @ApiModelProperty("修改人id")
    private Integer updateUser;

    @ApiModelProperty("是否删除：1是 0否")
    private Integer deleted;

    @ApiModelProperty("账号说明")
    private String accountDescription;
}

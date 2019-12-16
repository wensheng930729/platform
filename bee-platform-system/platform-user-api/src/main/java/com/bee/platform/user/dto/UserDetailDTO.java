package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 用户详细信息返回
 * @author: junyang.li
 * @create: 2019-03-22 09:20
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("用户详细信息返回")
public class UserDetailDTO implements Serializable {

    private static final long serialVersionUID = -3240548120937122266L;
    @ApiModelProperty("用户id")
    private Integer id;
    /**
     *
     */
    @ApiModelProperty("唯一标识")
    private String uuid;
    /**
     *
     */
    @ApiModelProperty("是否激活")
    private Integer isActive;
    /**
     *
     */
    @ApiModelProperty("头像")
    private String head;
    /**
     *
     */
    @ApiModelProperty("手机号")
    private String phone;
    /**
     *
     */
    @ApiModelProperty("邮箱")
    private String email;
    /**
     *
     */
    @ApiModelProperty("姓名")
    private String nickname;
    /**
     *
     */
    @ApiModelProperty("姓名拼音")
    private String nicknamePinyin;
    /**
     *
     */
    @ApiModelProperty("账号")
    private String username;

    /**
     *
     */
    @ApiModelProperty("用户名")
    private String consumerName;
    /**
     *
     */
    @ApiModelProperty("QQ")
    private String qq;
    /**
     *
     */
    @ApiModelProperty("县级地区id")
    private String regionid;
    /**
     *
     */
    @ApiModelProperty("详细地址")
    private String address;
    /**
     *
     */
    @ApiModelProperty("固话")
    private String fixtel;
}

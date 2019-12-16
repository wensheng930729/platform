package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName AuthUsergroupUserListDTO
 * @Description 功能描述
 * @Date 2019/7/17 15:50
 **/
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("用户组下的用户列表返回信息")
public class AuthUsergroupUserListDTO implements Serializable {
    private static final long serialVersionUID = 1L;
    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("用户名")
    private String name;
}

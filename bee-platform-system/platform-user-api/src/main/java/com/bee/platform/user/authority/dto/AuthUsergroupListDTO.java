package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "用户组列表DTO")
public class AuthUsergroupListDTO implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("用户组名")
    private String groupName;

    @ApiModelProperty("成员个数")
    private Integer userCount;

    @ApiModelProperty("描述")
    private String description;



}

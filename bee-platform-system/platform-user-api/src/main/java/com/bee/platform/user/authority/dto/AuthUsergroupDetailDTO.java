package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "用户组详情DTO")
public class AuthUsergroupDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("用户组名")
    private String groupName;

    @ApiModelProperty("描述")
    private String description;

    @ApiModelProperty("是否默认 0否 1是")
    private Integer defaultValue;


}

package com.bee.platform.user.authority.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName UserInterfaceUriDTO
 * @Description 功能描述
 * @Date 2019/5/27 15:02
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("用户下的接口url")
@JsonInclude
public class UserInterfaceUriDTO implements Serializable{
    private static final long serialVersionUID = -3792639954046571839L;
    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("url")
    private String url;

    @ApiModelProperty("subSys")
    private String subSys;
    
    @ApiModelProperty("请求类型")
    private String type;


}

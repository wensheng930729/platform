package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @Classname AuthUserEnterprisesDto
 * @Description 当前登录人企业信息返回DTO
 * @Date 2019/5/29 16:27
 * @Author liliang
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "当前登录人企业信息返回DTO")
public class AuthUserEnterprisesDto implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("用户电话")
    private String userNum;

    @ApiModelProperty("公司全称")
    private String name;

    @ApiModelProperty("地址")
    private String address;

    @ApiModelProperty("公司联系方式")
    private String contact;

    @ApiModelProperty("公司logo")
    private String head;

}

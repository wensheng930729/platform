package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @author liang.li
 * @ClassName MemberDto
 * @Description 用户id和姓名
 * @Date 2019-5-22
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("企业成员返回DTO")
public class MemberDto {

    @ApiModelProperty("用户id")
    private Integer id;

    @ApiModelProperty("用户姓名")
    private String name;

    @ApiModelProperty("用户账号")
    private String username;

}

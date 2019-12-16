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
 * @description: 用户管理数据库查询参数
 * @author: junyang.li
 * @create: 2019-03-20 14:31
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("用户管理数据库查询参数")
public class ManagerSearchParamDTO implements Serializable {

    private static final long serialVersionUID = -4524165815142259538L;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("用户账号")
    private String username;

    @ApiModelProperty("用户姓名")
    private String nickname;

    @ApiModelProperty("用户职位")
    private String position;
}

package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 
 * </p>
 *
 * @author liliang
 * @since 2019-04-28
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户管理列表查询参数")
public class UserManagerListRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("开始时间")
    private Date start;

    @ApiModelProperty("截止时间")
    private Date end;

    @ApiModelProperty("用户名")
    private String param;

    @ApiModelProperty("1用户名 2手机号 3企业名称 4产品 5全部")
    private Integer mode;


}

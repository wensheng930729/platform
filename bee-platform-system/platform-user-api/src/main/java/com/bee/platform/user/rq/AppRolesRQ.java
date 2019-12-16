package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName AppRolesRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/4/30$ 9:15$
 * @version 1.0.0
 */

@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AppRolesRQ  implements Serializable {

    private static final long serialVersionUID = -5161328453630278638L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("产品id")
    private Integer appId;

    @ApiModelProperty("角色名称")
    private String roleName;
}

package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName EnterprisesAppOpenRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/4/29$ 14:18$
 * @version 1.0.0
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业产品开通信息")
public class EnterprisesAppOpenRQ implements Serializable {

    private static final long serialVersionUID = -6312901903145275144L;

    @ApiModelProperty("产品id")
    private Integer appId;

    @ApiModelProperty("产品角色id")
    private Integer appRolesId;
}

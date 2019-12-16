package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName AuthResourceQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/27$ 15:50$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("菜单资源列表查询请求信息")
public class AuthResourceQueryRQ implements Serializable {

    private static final long serialVersionUID = -1682044712253928114L;

    @ApiModelProperty("菜单名称")
    private String resourceName;

    @ApiModelProperty("子系统类型")
    private String subSystemType;

    @ApiModelProperty("创建开始时间")
    private String createStartTime;

    @ApiModelProperty("创建截止时间")
    private String createEndTime;

}

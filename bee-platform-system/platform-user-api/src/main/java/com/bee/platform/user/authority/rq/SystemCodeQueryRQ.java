package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName SystemCodeQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 16:16$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("子系统查询请求信息")
public class SystemCodeQueryRQ implements Serializable {

    private static final long serialVersionUID = 2816123483212253043L;

    @ApiModelProperty("id/名称")
    private String text;

    @ApiModelProperty("开始时间")
    private String createStartTime;

    @ApiModelProperty("截止时间")
    private String createEndTime;
}

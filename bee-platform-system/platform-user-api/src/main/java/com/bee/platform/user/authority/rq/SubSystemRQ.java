package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName SubSystemRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/27$ 15:05$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("子系统配置请求信息")
public class SubSystemRQ implements Serializable {

    private static final long serialVersionUID = -4397616866369483929L;

    @ApiModelProperty("id")
    private String id;

    @ApiModelProperty("组id（不唯一）")
    private String sysGroupId;

    @ApiModelProperty("该组id下面的key(唯一)")
    private String sysCode;

    @ApiModelProperty("该码值的说明x信息")
    private String sysCodeVal;

    @ApiModelProperty("该码值的说明x信息")
    private String sysCodeDesc;
}

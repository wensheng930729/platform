package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;

/**
 * @Classname AuthInterfaceSelectRQ
 * @Description 接口条件查询传输对象
 * @Date 2019/5/20 15:57
 * @Author xin.huang
 */
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("接口条件查询传输对象")
public class AuthInterfaceSelectRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("资源名称")
    private String name;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("接口路由")
    private String beeRouter;

    @ApiModelProperty("创建时间")
    private String createTime;
}

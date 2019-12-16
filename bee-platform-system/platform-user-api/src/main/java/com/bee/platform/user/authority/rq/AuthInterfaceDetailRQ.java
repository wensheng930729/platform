package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName AuthInterfaceDetailRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/27$ 14:08$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("接口详细请求信息")
public class AuthInterfaceDetailRQ implements Serializable {

    private static final long serialVersionUID = 7974090752901744772L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("接口名称")
    private String name;

    @ApiModelProperty("类型")
    private String type;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("排序")
    private Integer orderNum;

    @ApiModelProperty("接口地址")
    private String url;
}

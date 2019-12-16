package com.bee.platform.user.authority.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @Classname AuthInterfaceDto
 * @Description 接口列表 返回数据
 * @Date 2019/5/20 16:29
 * @Author xin.huang
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "接口返回数据")
public class AuthInterfaceDto implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("资源id")
    private Integer id;

    @ApiModelProperty("资源名称")
    private String name;

    @ApiModelProperty("资源类型")
    private String type;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("排序")
    private Integer orderNum;

    @ApiModelProperty("资源地址")
    private String url;

    @ApiModelProperty("接口路由")
    private String beeRouter;

    @ApiModelProperty("状态")
    private Integer status;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("更新时间")
    private Date updateTime;

    @ApiModelProperty("是否开通")
    private boolean openStatu;
}

package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * @ClassName AuthResourceRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/20$ 15:58$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("资源详细请求信息")
public class AuthResourceRQ implements Serializable {

    private static final long serialVersionUID = -2659112067112427830L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("父id")
    private Integer pid;

    @NotEmpty
    @ApiModelProperty("子系统标识")
    private String subSys;

    @NotEmpty
    @ApiModelProperty("菜单名称")
    private String name;

    @ApiModelProperty("菜单类型")
    private Integer type;

    @ApiModelProperty("菜单图标")
    private String icon;

    @NotEmpty
    @ApiModelProperty("菜单url")
    private String path;

    @NotEmpty
    @ApiModelProperty("菜单component")
    private String component;

    @ApiModelProperty("菜单序号")
    private Integer orderNum;

    @ApiModelProperty("是否隐藏0展开1隐藏")
    private Integer hide;

    @ApiModelProperty("展示类型(0通用 1仅普通用户 2仅管理员)")
    private Integer showType;
}

package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName AppDetailRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/4/30$ 9:19$
 * @version 1.0.0
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AppDetailRQ implements Serializable {

    private static final long serialVersionUID = 7486519389148726841L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("角色")
    private List<AppRolesRQ> appRoleList;

    @ApiModelProperty("产品简介")
    private String introduction;
}

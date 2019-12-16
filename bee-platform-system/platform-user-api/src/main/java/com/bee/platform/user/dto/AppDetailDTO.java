package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName AppDetailDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/4/29$ 15:32$
 * @version 1.0.0
 */

@Data
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("企业产品信息")
public class AppDetailDTO implements Serializable {

    private static final long serialVersionUID = -6160806883196134402L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("产品名称")
    private String name;

    @ApiModelProperty("产品缩写")
    private String abbreviation;

    @ApiModelProperty("角色")
    private List<AppRolesDTO> appRoleList;

    @ApiModelProperty("产品简介")
    private String introduction;

    @ApiModelProperty("是否可以编辑（0不可以 1可以）")
    private Integer editStatu;
}

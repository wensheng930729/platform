package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-03-04
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "app")
public class AppDTO  implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("业务主键ID")
    private Integer id;

    @ApiModelProperty("logo图片地址URL")
    private String logo;

    @ApiModelProperty("secret")
    private String secret;

    @ApiModelProperty("app名称")
    private String name;

    @ApiModelProperty("企业app URL")
    private String url;

    @ApiModelProperty("企业ID")
    private Integer orgId;

    @ApiModelProperty(" 0: 未开通， 1：已开通")
    private Integer status;



}

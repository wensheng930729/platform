package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description:
 * @author: laixy
 * @create: 2019-4-29 13:59
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("注册企业信息")
public class EnterpriseInfoDTO implements Serializable  {

    private static final long serialVersionUID = 512518410420983852L;

    @ApiModelProperty("企业id")
    private Integer id;

    @ApiModelProperty("企业名称")
    private String name;

    @ApiModelProperty("企业logo")
    private String head;

    @ApiModelProperty("企业联系电话")
    private String contact;

    @ApiModelProperty("企业详细地址")
    private String address;

    @ApiModelProperty("申请时间")
    private Date createAt;
}

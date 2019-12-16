package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-18 13:54
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("用户企业的相关信息")
public class EnterpriseListDTO implements Serializable {

    private static final long serialVersionUID = 4360343721177853145L;

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

    @ApiModelProperty("企业部门数量")
    private Integer departmentNum;

    @ApiModelProperty("企业成员数量")
    private Integer userNum;
}

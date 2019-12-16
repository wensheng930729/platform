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
 * @description: 企业详细
 * @author: junyang.li
 * @create: 2019-03-22 09:47
 **/
@NoArgsConstructor
@Getter
@Setter
@ToString
@Accessors(chain = true)
@ApiModel("企业详细")
public class EnterpriseBasicDTO implements Serializable{

    private static final long serialVersionUID = -9112781471967154565L;
    @ApiModelProperty("企业id")
    private Integer id;

    @ApiModelProperty("企业名称")
    private String name;
}

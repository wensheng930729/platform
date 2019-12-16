package com.bee.platform.business.dto;

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
 * @description: 企业审核结果类型返回数据
 * @author: jie.zhang123
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("企业审核结果类型返回数据")
public class CheckTypeDTO implements Serializable{

    private static final long serialVersionUID = 8180475924685431734L;

    @ApiModelProperty("类型id")
    private Integer typeId;

    @ApiModelProperty("企业id")
    private String typeName;

    @ApiModelProperty("对应企业数")
    private Integer count;

}

package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;


@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("统计类")
public class CountDTO implements  Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "id")
    private Integer id;

    @ApiModelProperty(value = "统计字段的名称")
    private String countKey;

    @ApiModelProperty(value = "统计字段的数据")
    private String countValue;

    @ApiModelProperty(value = "统计字段的描述信息")
    private String countDesc;

    @ApiModelProperty(value = "统计字段的类型（在哪里显示）")
    private String countType;



}

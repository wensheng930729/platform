package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;


@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("版本号封装类")
public class VersionDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "版本号")
    private String version;

}

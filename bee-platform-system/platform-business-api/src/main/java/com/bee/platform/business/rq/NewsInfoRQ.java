package com.bee.platform.business.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "资讯信息")
public class NewsInfoRQ implements Serializable {


    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("标题")
    @NotEmpty(message = "标题不能为空！")
    private String title;

    @ApiModelProperty("内容")
    private String content;

    private String new_source;

    @ApiModelProperty("图片")
    private String image;

    @ApiModelProperty("类型")
    private Integer type;
}

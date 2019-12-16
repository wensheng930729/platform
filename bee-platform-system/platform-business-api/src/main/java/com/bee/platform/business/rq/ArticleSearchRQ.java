package com.bee.platform.business.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "文章查询条件")
public class ArticleSearchRQ implements Serializable {

    private static final long serialVersionUID = 4969152419260199039L;


    @ApiModelProperty("标题")
    private String title;

    @ApiModelProperty("类型")
    private Integer type;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("截止时间")
    private String endTime;
}

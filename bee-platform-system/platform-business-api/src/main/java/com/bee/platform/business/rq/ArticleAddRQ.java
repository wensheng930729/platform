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
@ApiModel(value = "公告编辑rq")
public class ArticleAddRQ implements Serializable {

    private static final long serialVersionUID = 4497506919378255182L;

    @ApiModelProperty("公告id 编辑传入")
    private Integer id;

    @ApiModelProperty("公告名")
    private String title;

    @ApiModelProperty("内容")
    private String content;

    @ApiModelProperty("类型")
    private Integer type;

    @ApiModelProperty("附件名")
    private String attachmentName;

    @ApiModelProperty("附件url")
    private String attachmentUrl;
}

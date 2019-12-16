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
 * @description: 文章返回数据
 * @author: jie.zhang123
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("文章返回数据")
public class ArticlesDTO implements Serializable{

    private static final long serialVersionUID = -4637937714023502678L;

    @ApiModelProperty("文章id")
    private Integer id;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("点击量")
    private Integer hits;
    
    @ApiModelProperty("文章标题")
    private String title;

    @ApiModelProperty("文章内容")
    private String content;

    @ApiModelProperty("文章发布日期")
    private Date createAt;

    @ApiModelProperty("文章类型")
    private Integer type;

    @ApiModelProperty("文章类型名称")
    private String typeName;

    @ApiModelProperty("部门名称")

    private String depName;
    @ApiModelProperty("发布人名称")
    private String username;

    @ApiModelProperty("附件名称")
    private String attachmentName;

    @ApiModelProperty("附件url")
    private String attachmentUrl;

    @ApiModelProperty("是否是最新的数据 0否 1是")
    private Integer newest;

    @ApiModelProperty("用户明")
    private String nickname;

}

package com.bee.platform.business.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author chengke
 * @version 1.0.0
 * @ClassName LearnDTO
 * @Description 功能描述
 * @Date 2019/3/18 9:39
 **/

@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("学习指南返回数据")
public class LearnDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("学习指南文章id")
    private Integer id;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("文章标题")
    private String title;

    @ApiModelProperty("文章内容")
    private String content;

    @ApiModelProperty("点击量")
    private Integer hits;

    @ApiModelProperty("文章发布日期")
    private Date createAt;

    @ApiModelProperty("文章修改日期")
    private Date updateAt;

    @ApiModelProperty("类型 使用指南 0 内部培训 1 平台信息 2 规章制度 3 ")
    private Integer type;

    @ApiModelProperty("状态 0 有效 1 无效")
    private Integer status;

    @ApiModelProperty("部门名称")
    private String depName;

    @ApiModelProperty("用户名字")
    private String userName;

}

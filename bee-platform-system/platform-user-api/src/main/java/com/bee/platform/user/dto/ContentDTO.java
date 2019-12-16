package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @ClassName: ContentDTO
 * @Description: 提交文章内容的实体类
 * @Author: fei.sun
 * @Date: 2019/5/5 15:48
 * @Version: 1.0
 */

@Data
@ApiModel("提交文章内容的实体类")
public class ContentDTO {

    @ApiModelProperty("文章内容主键id")
    private Integer id;

    @ApiModelProperty("文章标题")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("大类分类（1 代表常见问题大类，2代表用户指南大类）")
    private Integer classifyType;

    @ApiModelProperty("关联小类id")
    private Integer pId;

    @ApiModelProperty("移动端展示内容")
    private String mobileContent;

    @ApiModelProperty("pc端展示内容")
    private String pcContent;

    @ApiModelProperty("最近更新时间")
    private String updateTime;

    @ApiModelProperty("更新人")
    private String updateName;
}

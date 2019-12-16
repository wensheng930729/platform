package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.annotations.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.models.auth.In;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;

/**
 * @ClassName: ContentClassification
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/4/30 11:50
 * @Version: 1.0
 */
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain=true)
@TableName("m_content_classification")
@ApiModel(value = "文章内容")
public class ContentClassification {

    @ApiModelProperty("文章内容主键id")
    private Integer id;

    @ApiModelProperty("文章标题")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("关联小类id")
    private Integer pId;

    @ApiModelProperty("移动端展示内容")
    private String mobileContent;

    @ApiModelProperty("pc端展示内容")
    private String pcContent;

    @ApiModelProperty("状态标识，0代表使用中，1代表删除")
    private Integer status;

    @ApiModelProperty("创建人id")
    private Long createUid;

    @ApiModelProperty("创建时间")
    private LocalDateTime createTime;

    @ApiModelProperty("更新人id")
    private Long updateUid;

    @ApiModelProperty("更新时间")
    private LocalDateTime updateTime;

}

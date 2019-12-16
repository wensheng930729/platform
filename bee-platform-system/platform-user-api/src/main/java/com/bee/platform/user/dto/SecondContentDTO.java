package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @ClassName: SecondContentDTO
 * @Description: 小类列表下的常见问题实体类
 * @Author: fei.sun
 * @Date: 2019/5/5 16:33
 * @Version: 1.0
 */
@Data
@ApiModel("小类列表下的常见问题实体类")
public class SecondContentDTO {

    @ApiModelProperty("文章内容主键id")
    private Integer id;

    @ApiModelProperty("文章标题")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;
}

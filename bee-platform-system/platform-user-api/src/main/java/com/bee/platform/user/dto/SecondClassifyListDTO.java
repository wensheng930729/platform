package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @ClassName: SecondClassifyListDTO
 * @Description: 小类编辑列表实体类
 * @Author: fei.sun
 * @Date: 2019/5/5 16:30
 * @Version: 1.0
 */
@Data
@ApiModel("小类编辑列表实体类")
public class SecondClassifyListDTO {

    @ApiModelProperty("小类主键标识id")
    private Integer id;

    @ApiModelProperty("小类名称")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("最近更新时间")
    private String updateTime;

    @ApiModelProperty("更新人")
    private String updateName;

    @ApiModelProperty("常见问题列表")
    private List<SecondContentDTO> secondContentDTOs;
}

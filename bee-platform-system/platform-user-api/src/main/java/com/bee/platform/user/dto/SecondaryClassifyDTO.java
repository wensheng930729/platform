package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * @ClassName: SecondaryClassifyDTO
 * @Description: 系统帮助下 小类的请求实体
 * @Author: fei.sun
 * @Date: 2019/4/28 13:58
 * @Version: 1.0
 */

@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel(value = "添加小类的请求实体")
public class SecondaryClassifyDTO {

    @ApiModelProperty("小类主键标识id")
    private Integer id;

    @ApiModelProperty("小类名称")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

}

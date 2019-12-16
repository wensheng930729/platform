package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

/**
 * @ClassName: PrimaryClassifyListDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/4/28 17:09
 * @Version: 1.0
 */
@Getter
@Setter
@ToString
@ApiModel("大类分类的展示列表")
public class PrimaryClassifyListDTO {

    @ApiModelProperty("大类主键标识id")
    private Integer id;

    @ApiModelProperty("大类名称")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("大类分类，0代表帮助指南大类，1代表常见问题大类，2代表用户指南大类")
    private Integer classifyType;

    @ApiModelProperty("小类数量")
    private Integer secondClassifyNum;
}

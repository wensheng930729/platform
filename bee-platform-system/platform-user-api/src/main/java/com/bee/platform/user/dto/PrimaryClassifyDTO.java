package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.util.List;

/**
 * @ClassName: PrimaryClassifyDTO
 * @Description: 添加大类的请求实体类
 * @Author: fei.sun
 * @Date: 2019/4/28 11:17
 * @Version: 1.0
 */

@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel(value = "添加大类的请求实体")
public class PrimaryClassifyDTO {

    @ApiModelProperty("大类主键标识id")
    private Integer id;

    @ApiModelProperty("大类名称")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("大类分类，0代表帮助指南大类，1代表常见问题大类，2代表用户指南大类")
    private Integer classifyType;

    @ApiModelProperty("该大类下的小类")
    private List<SecondaryClassifyDTO> secondaryClassifies;
}

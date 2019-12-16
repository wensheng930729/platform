package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.util.List;

/**
 * @ClassName: HelperSubDTO
 * @Description: 帮助首页编辑大类提交实体类
 * @Author: fei.sun
 * @Date: 2019/4/30 14:29
 * @Version: 1.0
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel(value = "帮助首页编辑大类提交实体类")
public class HelperSubDTO {

    @ApiModelProperty("大类主键标识id")
    private Integer id;

    @ApiModelProperty("大类名称")
    private String name;

    @ApiModelProperty("显示排序数值")
    private Integer weights;

    @ApiModelProperty("大类分类，0代表帮助首页大类，1代表常见问题大类，2代表用户指南大类")
    private Integer classifyType;

    @ApiModelProperty("最近更新时间")
    private String updateTime;

    @ApiModelProperty("更新人")
    private String updateName;

    @ApiModelProperty("帮助首页编辑页面列表中的内容")
    private List<HelperContentDTO> helperContentDTOS;

}

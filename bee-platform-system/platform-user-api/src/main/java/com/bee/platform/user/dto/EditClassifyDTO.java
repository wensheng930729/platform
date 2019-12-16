package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * @ClassName: EditClassifyDTO
 * @Description: 常见问题或用户指南大类编辑 页面中实体类
 * @Author: fei.sun
 * @Date: 2019/5/5 9:41
 * @Version: 1.0
 */

@Data
@ApiModel("常见问题或用户指南大类编辑 页面中实体类")
public class EditClassifyDTO {

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

    @ApiModelProperty("大类编辑页面下的小类列表")
    private List<EditSecondClassifyDTO> editSecondClassifyDTOs;
}

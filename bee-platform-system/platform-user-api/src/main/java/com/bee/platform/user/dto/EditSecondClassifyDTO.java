package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @ClassName: EditSecondClassifyDTO
 * @Description: 常见问题或用户指南大类编辑下的小类列表
 * @Author: fei.sun
 * @Date: 2019/5/5 9:45
 * @Version: 1.0
 */

@Data
@ApiModel("常见问题或用户指南大类编辑下的小类列表")
public class EditSecondClassifyDTO {

    @ApiModelProperty("小类主键id")
    private Integer sId;

    @ApiModelProperty("小类名称")
    private String sName;

    @ApiModelProperty("小类显示排序数值")
    private Integer sWeights;

    @ApiModelProperty("小类下的文章数")
    private Integer contentNum;

}

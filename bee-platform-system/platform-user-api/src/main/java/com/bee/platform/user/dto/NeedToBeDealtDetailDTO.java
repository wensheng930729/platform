package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @ClassName NeedToBeDealtDetailDTO
 * @Description 待办详细信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/5/15 14:39
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("待办详细信息")
public class NeedToBeDealtDetailDTO {

    @ApiModelProperty("待办任务编号 ")
    private String taskNumber;

    @ApiModelProperty("待办任务标题")
    private String taskTitle;

    @ApiModelProperty("待办任务内容")
    private String taskContent;

    @ApiModelProperty("待办任务类型 1工单、2工作台")
    private Integer taskType;

    @ApiModelProperty("处理时间")
    private Date handleTime;

}

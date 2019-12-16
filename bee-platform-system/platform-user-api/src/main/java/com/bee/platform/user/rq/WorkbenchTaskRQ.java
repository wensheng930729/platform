package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName WorkbenchTaskRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/6$ 14:56$
 * @version 1.0.0
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "工作台任务查询条件")
public class WorkbenchTaskRQ implements Serializable {

    private static final long serialVersionUID = 3738991766132008298L;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("截止时间")
    private String endTime;

    @ApiModelProperty("任务类型 1线上蜂贸、2蜂创物联、3领蜂供应链、4集蜂联运、5金蜜ERP")
    private Integer taskType;

    @ApiModelProperty("处理状态 0待处理 1已处理")
    private Integer taskStatu;
}

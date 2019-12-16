package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName NeedToBeDealtDTO
 * @Description 待办相关信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/5/15 14:39
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("待办相关信息")
public class NeedToBeDealtDTO {

    @ApiModelProperty("待办任务数量")
    private Integer taskNum;

    @ApiModelProperty("待办任务详情列表")
    private List<NeedToBeDealtDetailDTO> detailDTOS;

}

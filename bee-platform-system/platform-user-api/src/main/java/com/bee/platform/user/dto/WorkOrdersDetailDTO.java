package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

/**
 * @ClassName WorkOrdersDetailDTO
 * @Description 工单沟通详情信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/4/25 14:41
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("工单沟通详情信息")
public class WorkOrdersDetailDTO {

    @ApiModelProperty("工单编号")
    private String workOrderNumber;

    @ApiModelProperty("回复详情")
    private String replyDescription;

    @ApiModelProperty("附件")
    private List<AttachmentDTO> enclosure;

    @ApiModelProperty("回复人id")
    private Long replyId;

    @ApiModelProperty("回复人")
    private String replyName;

    @ApiModelProperty("回复时间")
    private Date replyTime;

    @ApiModelProperty("回复人头像")
    private String replyHead;

}

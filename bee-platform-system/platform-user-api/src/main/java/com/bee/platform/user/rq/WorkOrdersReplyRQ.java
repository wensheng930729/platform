package com.bee.platform.user.rq;

import com.bee.platform.user.dto.AttachmentDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * @ClassName WorkOrdersReplyRQ
 * @Description 工单沟通回复信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/4/25 15:54
 */
@Data
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("工单沟通回复信息")
public class WorkOrdersReplyRQ implements Serializable {

    private static final long serialVersionUID = 3318914463969929005L;

    @ApiModelProperty("工单编号 ")
    @NotEmpty(message = "工单编号不能为空")
    private String workOrderNumber;

    @ApiModelProperty("回复详情")
    @NotEmpty(message = "回复详情不能为空")
    @Length(max = 1000,message = "回复详情限制1000个字符")
    private String replyDescription;

    @ApiModelProperty("附件")
    private List<AttachmentDTO> enclosure;

}

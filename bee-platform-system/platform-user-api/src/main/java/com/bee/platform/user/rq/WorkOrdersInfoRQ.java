package com.bee.platform.user.rq;

import com.bee.platform.user.dto.AttachmentDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @ClassName WorkOrdersInfoRQ
 * @Description 新建工单信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/4/25 15:54
 */
@Data
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("新建工单信息")
public class WorkOrdersInfoRQ implements Serializable {

    private static final long serialVersionUID = 5195892924825416528L;

    @ApiModelProperty("优先级2重要1一般")
    @NotNull(message = "优先级不能为空")
    private Integer priority;

    @ApiModelProperty("问题所属产品")
    @NotNull(message = "问题所属产品不能为空")
    private Integer belongApp;

    @ApiModelProperty("工单标题")
    @NotEmpty(message = "工单标题不能为空")
    private String workOrderTitle;

    @ApiModelProperty("问题描述")
    @NotEmpty(message = "问题描述不能为空")
    @Length(max = 1200,message = "问题描述限制1200个字符")
    private String problemDescription;

    @ApiModelProperty("联系手机号码")
    @NotEmpty(message = "手机号不能为空")
    private String phone;

    @ApiModelProperty("附件")
    private List<AttachmentDTO> enclosure;

}

package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 
 * </p>
 *
 * @author liliang
 * @since 2019-04-28
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "意见反馈入参")
public class FeedbackRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("建议类型(0产品BUG 1功能建议)")
    private Integer adviceType;

    @ApiModelProperty("建议标题")
    @NotEmpty(message = "标题不能为空")
    private String adviceTitle;

    @ApiModelProperty("意见内容")
    @NotEmpty(message = "内容不能为空")
    private String content;

    @ApiModelProperty("附件")
    private List<FileRQ> files;

}

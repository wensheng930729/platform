package com.bee.platform.user.dto;

import com.bee.platform.user.rq.FileRQ;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
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
@ApiModel(value = "意见反馈详情dto")
public class FeedbackDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("业务主键ID")
    private Integer id;

    @ApiModelProperty("建议人")
    private String adviceUser;

    @ApiModelProperty("建议类型")
    private Integer adviceType;

    @ApiModelProperty("建议类型String")
    private String adviceTypeString;

    @ApiModelProperty("建议标题")
    private String adviceTitle;

    @ApiModelProperty("意见内容")
    private String content;

    @ApiModelProperty("附件")
    private List<FileRQ> files;

    @ApiModelProperty("提交时间")
    private Date createTime;




}

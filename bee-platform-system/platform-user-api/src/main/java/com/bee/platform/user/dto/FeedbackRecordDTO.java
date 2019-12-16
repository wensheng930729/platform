package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

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
@ApiModel(value = "意见反馈记录dto")
public class FeedbackRecordDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("业务主键ID")
    private Integer id;

    @ApiModelProperty("建议人")
    private String adviceUser;

    @ApiModelProperty("反馈状态：是否查看（0未查看 1查看）")
    private Integer isCheck;

    @ApiModelProperty("提交时间")
    private Date createTime;



}

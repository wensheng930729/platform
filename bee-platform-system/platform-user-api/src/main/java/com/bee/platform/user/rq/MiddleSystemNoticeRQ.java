package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * <p>
 * 中台系统通知
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-09
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "系统通知请求参数")
public class MiddleSystemNoticeRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 通知人id
     */
    @ApiModelProperty("通知人id")
    @NotNull(message = "通知人id不能为空")
    private Integer notifierId;
    /**
     * 通知标题
     */
    @ApiModelProperty("通知标题")
    @NotEmpty(message = "通知标题不能为空")
    private String title;
    /**
     * 通知内容
     */
    @ApiModelProperty("通知内容")
    @NotEmpty(message = "通知内容不能为空")
    private String content;




}

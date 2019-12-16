package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author chengke
 * @version 1.0.0
 * @ClassName MiddleSystemNoticeDTO
 * @Description 功能描述
 * @Date 2019/5/9 16:45
 **/
@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("中台系统通知信息返回数据")
public class MiddleSystemNoticeDTO implements Serializable{


    /**
	 * 
	 */
	private static final long serialVersionUID = 3416909062636613653L;

	@ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("通知人id")
    private Integer notifierId;
    /**
     * 通知标题
     */
    @ApiModelProperty("通知标题")
    private String title;
    /**
     * 通知内容
     */
    @ApiModelProperty("通知内容")
    private String content;
    /**
     * 是否阅读，0未读，1已读
     */
    @ApiModelProperty("是否阅读，0未读，1已读")
    private Integer isRead;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;


}

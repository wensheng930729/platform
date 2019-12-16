package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * @notes: 后台管理系统通知
 * @Author: junyang.li
 * @Date: 10:16 2019/5/6
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("后台管理系统通知")
public class SystemNoticeDTO  {


    @ApiModelProperty("通知id")
    private Long noticeId;

    @ApiModelProperty("通知人id")
    private Integer notifierId;

    @ApiModelProperty("通知标题")
    private String noticeTitle;

    @ApiModelProperty("通知内容")
    private String noticeContent;

    @ApiModelProperty("是否阅读，0未读，1已读")
    private Integer read;

    @ApiModelProperty("是否有效，0无效，1有效")
    private Integer status;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("修改时间")
    private Date updateTime;


}

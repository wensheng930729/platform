package com.bee.platform.user.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * @description: 系统通知全部已读
 * @author: junyang.li
 * @create: 2019-05-20 09:39
 **/
@Getter
@Setter
@NoArgsConstructor
@ToString
@Accessors(chain = true)
@ApiModel("系统通知全部已读")
public class SystemNoticeVO {

    @ApiModelProperty("通知id")
    @NotEmpty(message = "通知编号不能为空")
    private List<Integer> noticeIds;
}

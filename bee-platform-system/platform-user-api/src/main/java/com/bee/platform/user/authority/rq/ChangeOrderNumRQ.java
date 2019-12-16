package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ChangeOrderNumRQ
 * @Description 功能描述
 * @Date 2019/5/21 16:35
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("改版排序字段请求参数")
public class ChangeOrderNumRQ {

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("num")
    private Integer num;
}

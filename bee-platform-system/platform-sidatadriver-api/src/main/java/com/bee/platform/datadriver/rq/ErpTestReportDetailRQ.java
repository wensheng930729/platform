package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName ErpTestReportDetailRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/6$ 10:18$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("化验单详情请求信息")
public class ErpTestReportDetailRQ implements Serializable {

    private static final long serialVersionUID = 233627208728909148L;

    /*@ApiModelProperty("化验单id")
    private Integer testId;*/

    @ApiModelProperty("化验项目")
    private String testItem;

    @ApiModelProperty("化验值")
    private String testValue;

    @ApiModelProperty("是否默认显示:0-不显示 1-显示")
    private Integer show;
}

package com.bee.platform.datadriver.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName ErpTestReportDetailDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/6$ 11:46$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("化验单项目信息")
public class ErpTestReportDetailDTO implements Serializable {

    private static final long serialVersionUID = -8516291112063250495L;

    @ApiModelProperty("化验项目")
    private String testItem;

    @ApiModelProperty("化验值")
    private String testValue;

    @ApiModelProperty("是否默认显示:0-不显示 1-显示")
    private Integer show;
}

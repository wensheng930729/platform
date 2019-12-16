package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName TestReportTypeRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 15:22$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("化验类型保存请求信息")
public class TestReportTypeRQ implements Serializable {

    private static final long serialVersionUID = -625505262534948351L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("化验类别:值来自码表assay_category")
    private Integer type;

    @ApiModelProperty("化验类型名称")
    private String name;

    @ApiModelProperty("1-启用,0-停用")
    private Integer status;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    /*@ApiModelProperty("企业id-前端不传")
    private List<Integer> list;*/
}

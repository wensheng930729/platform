package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @ClassName TestReportQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 9:31$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("化验单列表查询请求信息")
public class TestReportQueryRQ implements Serializable {

    private static final long serialVersionUID = -8560600640975599369L;

    @ApiModelProperty("公司id")
    private Integer company;

    @ApiModelProperty("化验单类型")
    private Integer testType;

    @ApiModelProperty("化验单编号")
    private String testCode;

    @ApiModelProperty("化验单产品")
    private String testProduct;

    @ApiModelProperty("化验人")
    private String testUser;

    @ApiModelProperty("状态")
    private Integer status;

    @ApiModelProperty("开始时间")
    private String createStartTime;

    @ApiModelProperty("截止时间")
    private String createEndTime;

    /*@ApiModelProperty("企业id-前端不传")
    private List<Integer> list;*/

}

package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName orderQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/28$ 17:41$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("订单查询请求信息")
public class DinasOrderQueryRQ implements Serializable {

    private static final long serialVersionUID = -6361616073471730137L;

    @ApiModelProperty("合同编号")
    private String code;

    @ApiModelProperty("供应商")
    private Integer customerId;

    @ApiModelProperty("供应商")
    private String customerName;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("开始时间")
    private String createStartTime;

    @ApiModelProperty("截止时间")
    private String createEndTime;
}

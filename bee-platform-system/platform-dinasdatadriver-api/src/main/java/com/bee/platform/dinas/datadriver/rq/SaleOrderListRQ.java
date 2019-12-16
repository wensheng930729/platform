package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author xin.huang
 * @description
 * @date 2019/8/13
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售合同列表查询请求信息")
public class SaleOrderListRQ implements Serializable {
    private static final long serialVersionUID = -216320676816851679L;

    @ApiModelProperty("合同编号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("订货商名称")
    private String customerName;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("合同开始日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDateStart;

    @ApiModelProperty("合同结束日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date contractDateEnd;
}

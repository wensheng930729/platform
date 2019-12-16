package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description 销售回款请求信息
 * @date 2019/8/14
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售回款请求信息")
public class SalePaymentRQ implements Serializable {
    private static final long serialVersionUID = 790908409554599913L;

    @ApiModelProperty("回款id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("回款单号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("回款金额")
    private BigDecimal receiveAmount;

    @ApiModelProperty("回款日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiveDate;

    @ApiModelProperty("回款附件")
    private List<DinasUrlRQ> urlList;
}

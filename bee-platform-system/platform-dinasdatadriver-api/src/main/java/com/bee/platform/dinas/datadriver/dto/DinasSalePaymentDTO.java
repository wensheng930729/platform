package com.bee.platform.dinas.datadriver.dto;

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
 * @description 销售回款详情返回信息
 * @date 2019/8/15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("销售回款详情返回信息")
public class DinasSalePaymentDTO implements Serializable {
    private static final long serialVersionUID = 1686880869922590222L;

    @ApiModelProperty("回款id")
    private Integer id;

    @ApiModelProperty("合同id")
    private Integer orderId;

    @ApiModelProperty("合同编号")
    private String orderCode;

    @ApiModelProperty("回款单号")
    private String code;

    @ApiModelProperty("订货商id")
    private Integer customerId;

    @ApiModelProperty("订货商名称")
    private String customerName;

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
    private String url;

    @ApiModelProperty("回款附件列表")
    private List<DinasUrlDTO> urlList;
}
